(ns lambdaconnect-graphql.utils
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [failjure.core :as f]
            [clj-time.format :as time-format]
            [lambdaconnect-model.tools :as tools]) 
  (:import java.text.SimpleDateFormat
           [java.util TimeZone]))

(defn now [] (java.util.Date.))

(defn uuid [] (java.util.UUID/randomUUID))

(defn deep-merge [& maps]
  (letfn [(reconcile-keys [val-in-result val-in-latter]
            (if (and (map? val-in-result)
                     (map? val-in-latter))
              (merge-with reconcile-keys val-in-result val-in-latter)
              val-in-latter))
          (reconcile-maps [result latter]
            (merge-with reconcile-keys result latter))]
    (reduce reconcile-maps maps)))

(defn format-date
  [java-util-date]
  (let [fmt (doto (SimpleDateFormat. "yyyy-MM-dd'T'HH:mm:ss.SSSZ")
              (.setTimeZone (TimeZone/getTimeZone "UTC")))]
    (.format fmt java-util-date)))

(defn fail
  [message data]
  (when data (assoc (f/fail message) :error data)))

(defn fail-many
  ([errors]
   (some->> errors (keep identity) seq f/fail))
  ([message errors]
   (some->> errors (keep identity) seq (fail message))))

(defn reduce-failure
  "Improved clojure.core/reduce that short-circuits if reduce function returns failjure fail."
  [f val coll]
  (reduce (fn [coll new-obj] (f/attempt reduced (f coll new-obj))) val coll))

(defn reduce-failures
  "Improved clojure.core/reduce that collects all failjure fails."
  [f val coll]
  (let [g (fn [[errors coll] new-obj]
            (f/if-let-ok? [x (f coll new-obj)]
                          [errors x]
                          (let [e (:message x)]
                            [((if (string? e) conj into) errors e) coll])))
        [errors result] (reduce g [[] val] coll)]
    (if (seq errors) (f/fail errors) result)))

(defn map-failures
  [f coll]
  (reduce-failures (fn [acc x] (f/ok->> (f x) (conj acc))) [] coll))

(defn update-in-failure
  "Improved clojure.core/update-in that, if updating function fails, returns that failure instead of original map."
  [m ks f & args]
  (f/if-let-ok? [v (apply f (get-in m ks) args)] (assoc-in m ks v) v))

(defn failure->lacinia-error
  [{:keys [resolve-as]} {:keys [message error]}]
  (resolve-as nil (merge {:message (if (string? message) message (str/join "\n" message))
                          :status 400}
                         (when error {:extensions {:error error}}))))

(defn string->uuid
  [s]
  (if (uuid? s) s
      (do
        (assert (string? s) (str "Not a string passed as a UUID: " s))
        (. java.util.UUID fromString s))))

(defn union
  [els-or-sets]
  (disj (reduce (fn [set el-or-set]
                  (if (set? el-or-set)
                    (set/union set el-or-set)
                    (conj set el-or-set)))
                #{} els-or-sets) nil))

(defn field-parser
  [type]
  (case type
    :db.type/string identity
    :db.type/instant #(->> %
                           (time-format/parse tools/time-formatter)
                           (tools/to-database-date))
    :db.type/boolean #(case % "true" true "false" false)
    :db.type/long #(Long. %)
    :db.type/float #(Float. %)
    :db.type/double #(Double. %)
    :db.type/uuid string->uuid))

(defn ->datomic-keyword
  "Convert entry in entities-by-name/RAEntity/:datomic-relationships/attribute to a Datomic attribute."
  [r]
  (when r (keyword (:entity-name r) (:name r))))

(defn new-entity
  "If creating a model entity (entity-name is string),
   data is NOT used to create datoms for all attributes,
   but only to select default values for missing attributes.
   If creating another entity (entity-name is nil, example: upload),
   all data is used.
   In general, take-all-data? should be set to false when called by mutation (create_upsert)
   and true otherwise. Lower arities are for convenience only."
  ([config entity-name now data] (new-entity config entity-name now data false))
  ([{:keys [default-values]} entity-name now data take-all-data?]
   (let [data (->> data (filter (fn [[_ v]] v)) (into {}))]
     (merge (if entity-name
              (let [default-attributes (get default-values entity-name)
                    default-keys (set (keys default-attributes))
                    present-keys (set (keys data))
                    missing-keys (set/difference default-keys present-keys)]
                (merge (if take-all-data?
                         data
                         (select-keys data (concat [:db/id] default-keys)))
                       (select-keys default-attributes missing-keys)))
              data)
            {:app/active true
             :app/uuid (or (:app/uuid data) (uuid))
             :app/createdAt now
             :app/updatedAt now}
            (when entity-name
              {(keyword entity-name "ident__") true})))))
