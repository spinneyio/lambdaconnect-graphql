(ns lambdaconnect-graphql.mutation
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [failjure.core :as f]
            [lambdaconnect-graphql.constants :as constants]
            [lambdaconnect-graphql.log :as log]
            [lambdaconnect-graphql.model :as model]
            [lambdaconnect-graphql.patterns :as patterns]
            [lambdaconnect-graphql.pull :as pull]
            [lambdaconnect-graphql.relations :as relations]
            [lambdaconnect-graphql.scope :as scope]
            [lambdaconnect-graphql.utils :as u]
            [lambdaconnect-model.core :as lm]
            [lambdaconnect-model.tools :as t]))

(defn parser-for-input-relationship
  [rel]
  (if (:to-many rel)
    (let [f #(map (fn [uuid-string] {:app/uuid (t/string->uuid uuid-string)}) %)]
      #(some-> % (update :add f) (update :remove f)))
    #(when % {:app/uuid (t/string->uuid %)})))

(defn json-to-clojure
  "Converts json-based map into the one that conforms to the spec."
  [json entity]
  (lm/json-to-clojure json entity t/parser-for-attribute parser-for-input-relationship))

(defn parse-input
  "We have input (data) like this:
   {NOUser: {birthDate: \"2012-12-19T06:01:17.171Z\",
             email: \"test@test.pl\",
             fullName: \"Some Name\",
             client: {dietGoal: \"Some goal\",
                      uuid: \"uuid-of-client\"
                      weightHistory: {add: [{weight: 100,
                                             measuredAt: \"2012-12-19T06:01:17.171Z\",
                                             uuid: \"uuid-of-weight-history\"}]}}}
   And we want to parse it to obtain push-input form e.g.
   {\"NOUser\" [{\"birthDate\" \"2012-12-19T06:01:17.171Z\",
                 \"email\" \"test@test.pl\",
                 \"fullName\" \"Some Name\",
                 \"client\" \"uuid-of-client\"}],
    \"NOClient\" [{\"dietGoal\" \"Some goal\",
                   \"uuid\" \"uuid-of-client\"
                   \"weightHistory\" {:add [\"uuid-of-weight-history\"]
                                      :remove []}],
    \"NOWeightHistory\" [{\"weight\" 100,
                          \"measuredAt\" \"2012-12-19T06:01:17.171Z\"
                          \"uuid\" \"uuid-of-weight-history\"}]
   We parse it recursively (DFS), with new recursive call for each relationship."
  [data entity {:keys [entities-by-name] :as config}]
  (if (and (= (count data) 1) (:uuid data))
    {(:name entity) [{"uuid" (:uuid data)}]}
    (let [push-input (atom {}) ; We save results of parsed relationships in push-input atom!
          parse-to-many-list (fn [relationship-value destination-entity]
                               (->> relationship-value
                                    (map #(parse-input % destination-entity config))
                                    (apply (partial merge-with concat))))
          parse-to-many-input (fn [relationship-input destination-entity]
                                (-> relationship-input
                                    (update :add parse-to-many-list destination-entity)
                                    (update :remove parse-to-many-list destination-entity)))
          parsed-data (->> data
                           (map (fn [[field value]]
                                  (let [relationship (get (:relationships entity) (name field))
                                        destination (:destination-entity relationship)
                                        destination-entity (get entities-by-name destination)]
                                    (cond
                                      (or (nil? relationship) (nil? value))
                                      [(name field) value]

                                      (:to-many relationship)
                                      (let [parsed-values (parse-to-many-input value destination-entity)]
                                        (swap! push-input #(merge-with concat % (:add parsed-values) (:remove parsed-values)))
                                        [(name field) {:add (map :uuid (:add value))
                                                       :remove (map :uuid (:remove value))}])

                                      :else
                                      (let [parsed-values (parse-input
                                                           value
                                                           destination-entity
                                                           config)]
                                        (swap! push-input #(merge-with concat % parsed-values))
                                        [(name field) (:uuid value)])))))
                           doall
                           (into {}))]
      (merge-with concat @push-input {(:name entity) (list parsed-data)}))))

(defn validate-value
  [entities-by-name entity uuid attribute value]
  (let [attribute (name attribute)]
    (when value
      (concat
       (when-let [{:keys [regular-expression min-value max-value type]}
                  (get-in entities-by-name [entity :attributes attribute])]
         [(when (and regular-expression
                     (or (not (string? value))
                         (not (re-matches regular-expression value))))
            (str entity "/" attribute " (uuid: " uuid "): value must match regex " regular-expression "."))
          (when min-value
            (case type
              :db.type/string
              (when (< (count value) min-value)
                (str entity "/" attribute " (uuid: " uuid "): value must have at least " min-value " characters."))
              :db.type/long
              (when (< value min-value)
                (str entity "/" attribute " (uuid: " uuid "): value must be at least " min-value "."))
              nil))
          (when max-value
            (case type
              :db.type/string
              (when (> (count value) max-value)
                (str entity "/" attribute " (uuid: " uuid "): value must have at most " min-value " characters."))
              :db.type/long
              (when (> value max-value)
                (str entity "/" attribute " (uuid: " uuid "): value must be at most " min-value "."))
              nil))])))))

(defn add-datom
  "Datoms should have form [:db/{add,retract} n-identifier ...]"
  [[updated-n-identifiers datoms] new-datom]
  [(conj updated-n-identifiers (second new-datom))
   (conj datoms new-datom)])

(defn add-n-identifiers
  [[updated-n-identifiers datoms] new-updated-n-identifiers]
  [(into updated-n-identifiers new-updated-n-identifiers)
   datoms])

(defn ->datoms-only
  [now [updated-n-identifiers datoms]]
  (into (mapv (fn [n-id] [:db/add n-id :app/updatedAt now])
              updated-n-identifiers)
        datoms))

(defn expected-types
  "Return list of pairs [uuid, expected type of entity with given uuid (string)]"
  [{:keys [entities-by-name]} entity object]
  (let [entity-description (get entities-by-name entity)]
    (mapcat (fn [[k val]]
              (if (= :app/uuid k)
                [[val entity]]
                (when-let [rel (get-in entity-description [:relationships (name k)])]
                  (if (:to-many rel)
                    (map (fn [id] [(:app/uuid id) (:destination-entity rel)])
                         (concat (:add val) (:remove val)))
                    (when-let [uuid (:app/uuid val)]
                      [[uuid (:destination-entity rel)]])))))
            object)))

(defn add-and-remove
  "Input: mutation input value for to-many relationship, either
   as map with keys :add and :remove or two distinct collections.

   Output: map with keys :add and remove if there are no conflicts or failjure error."
  ([m] (add-and-remove (:add m) (:remove m)))
  ([add remove]
   (let [add (set add)
         remove (set remove)
         both (set/intersection add remove)]
     (or (when (seq both)
           (f/fail (str "Mutation is trying to add and remove relation at the same time with objects "
                        (str/join ", " (map :app/uuid both)) ".")))
         {:add (vec add) :remove (vec remove)}))))

(defn add-remove-map?
  [m]
  (and (map? m) (seq (set/intersection #{:add :remove} (set (keys m))))))

(defn merge-values
  "Input: two mutation input values for the same field.

   Output: union of values if they are not conflicting or failjure error.
   In case of scalars, objects must be equal.
   In case of add/remove maps, they must have empty intersection
   (either add with either remove) and their union is union of add/remove lists."
  [v1 v2]
  (cond (f/failed? v1) v1
        (f/failed? v2) v2

        (or (and (nil? v1) (not (nil? v2)))
            (and (nil? v2) (not (nil? v1))))
        (f/fail "Mutation is trying to update and delete value at the same time.")

        (and (nil? v1) (nil? v2)) nil

        (and (add-remove-map? v1) (add-remove-map? v2))
        (add-and-remove (concat (:add v1) (:add v2)) (concat (:remove v1) (:remove v2)))

        (not= v1 v2)
        (f/fail "Mutation is trying to update with two different values.")

        :else v1))

(defn parse-flat-input
  "Input: entities-by-name, flat-input (result of parse-input flattened:
   each element has shape [entity-name map-with-values] instead of [entity-name list-of-maps]).

   Output: list of objects in mutation, each represented as vector [entity-name map-with-values].
   In case of unexpected errors (such as invalid UUID), this function failjures."
  [{:keys [entities-by-name]} flat-input]
  (u/reduce-failures (fn [parsed-input [entity object]]
                       (try (->> (get entities-by-name entity)
                                 (json-to-clojure object)
                                 (vector entity)
                                 (conj parsed-input))
                            (catch Exception e
                              (f/fail (.getMessage e)))))
                     []
                     flat-input))

(defn add-detached-objects
  "When updating to-one relationship, it can be presented as either 1 uuid or nil.
   In either case, replace value with map
   {:add [uuid] (if value is uuid)
    :remove [other-uuid] (if replacing or removing existing relation, uuid of old object)}.

   When updating to-many relationship, it can be presented as either
   map {:add [list of uuids] :remove [list of uuids]} or nil (delete from all objects).
   In that case, replace nil with {:add [] :remove [all objects connected and accesible by scoping]}.

   Additionally, add objects that are on the other side of all mentioned relations
   (to-one and to-many) before applying mutation to flat-parsed-input."
  [{:keys [entities-by-name q]} flat-parsed-input db user-scoped-ids]
  (let [queries-description ;; [entity-name datomic-attribute inverse-entity namespaced-attribute to-many entity-uuid]
        (mapcat (fn [[entity object]]
                  (let [entity-description (get entities-by-name entity)
                        uuid (:app/uuid object)]
                    (keep (fn [[namespaced-attribute value]]
                            (let [attribute (name namespaced-attribute)

                                  {:keys [inverse-entity inverse-name to-many] :as relation}
                                  (get-in entity-description [:relationships attribute])]
                              (when (and relation (or (not to-many) (nil? value)))
                                (let [datomic-relation
                                      (get-in entity-description [:datomic-relationships attribute])

                                      inverse-datomic-relation
                                      (get-in entities-by-name [inverse-entity :datomic-relationships inverse-name])]
                                  [entity
                                   (or (u/->datomic-keyword datomic-relation)
                                       (u/->datomic-keyword inverse-datomic-relation))
                                   inverse-entity
                                   namespaced-attribute
                                   to-many
                                   uuid]))))
                          object)))
                flat-parsed-input)

        other-id (fn [i] (symbol (str "?other-id-" i)))

        queries-result ;; {uuid {attr {:entity "RAEntity" :to-many bool :uuids [{:app/uuid uuid}]}}}
        (->> queries-description
             (group-by first)
             (pmap (fn [[entity-name queries-to-do]]
                     (let [entity-description (get entities-by-name entity-name)
                           uuids (set (map last queries-to-do))
                           datomic-attributes (->> queries-to-do
                                                   (map (fn [[_ datomic-attr inverse-entity namespaced-attr]]
                                                          [datomic-attr {:entity inverse-entity
                                                                         :namespaced-attribute namespaced-attr
                                                                         :forward? (= (namespace datomic-attr) entity-name)}]))
                                                   (into {})
                                                   (map-indexed (fn [i [datomic-attr query-data]]
                                                                  (assoc query-data
                                                                         :datomic-attr datomic-attr
                                                                         :other-id (other-id i)))))
                           relation->inverse-entity (->> datomic-attributes (map (juxt :namespaced-attribute :entity)) (into {}))

                           get-attribute-rule (fn [relation forward? other-id]
                                                `(~'or-join [~'?id ~other-id]
                                                            ~(if forward?
                                                               ['?id relation other-id]
                                                               [other-id relation '?id])
                                                            [(~'ground ~constants/empty-value) ~other-id]))
                           ;; [{:uuid entity-uuid :related-entity-1 #{related-entity-1-id OR empty-value} :related-entity-N #{related-entity-N-id OR empty-value}}]
                           unscoped-ids (q `[:find ~'?uuid ~@(map #(list 'set (:other-id %)) datomic-attributes)
                                             :keys ~'uuid ~@(map (comp symbol :namespaced-attribute) datomic-attributes)
                                             :in ~'$ ~'[?uuid ...]
                                             :where
                                             (~'or-join [~'?uuid ~@(map :other-id datomic-attributes)]
                                                        (~'and ~'[?id :app/uuid ?uuid]
                                                               ~@(map (comp (partial apply get-attribute-rule)
                                                                            (juxt :datomic-attr :forward? :other-id))
                                                                      datomic-attributes))
                                                        (~'and ~'(not [_ :app/uuid ?uuid])
                                                               [(~'ground ~constants/empty-value) ~'?id]
                                                               ~@(map #(vector (list 'ground constants/empty-value) (:other-id %))
                                                                      datomic-attributes)))]
                                           db uuids)
                           scoped-id->uuid (->> unscoped-ids
                                                (map #(dissoc % :uuid))
                                                (apply merge-with set/union)
                                                (map (fn [[namespaced-attr ids]]
                                                       (set/intersection @(user-scoped-ids (relation->inverse-entity namespaced-attr)) ids)))
                                                (apply set/union)
                                                (q '[:find ?id ?uuid
                                                     :in $ [?id ...]
                                                     :where [?id :app/uuid ?uuid]] db)
                                                (into {}))
                           scoped-uuids (map (fn [response-row]
                                               (as-> response-row $
                                                 (dissoc $ :uuid)
                                                 (map (fn [[attr ids]] [attr (keep scoped-id->uuid ids)]) $)
                                                 (into {} $)
                                                 (assoc $ :uuid (:uuid response-row))))
                                             unscoped-ids)
                           query-result
                           (->> scoped-uuids
                                (map (fn [result-map]
                                       [(:uuid result-map)
                                        (->> (dissoc result-map :uuid)
                                             (map (fn [[attr entities]]
                                                    (let [{:keys [to-many inverse-entity]}
                                                          (get-in entity-description [:relationships (name attr)])]
                                                      [attr
                                                       {:to-many to-many
                                                        :entity
                                                        (if (= (namespace attr) entity-name)
                                                          inverse-entity
                                                          (namespace attr))
                                                        :uuids (mapv #(hash-map :app/uuid %) entities)}])))
                                             (into {}))]))
                                (into {}))]
                       query-result)))
             (apply merge))

        updated-objects
        (map (fn [[entity object]]
               [entity (->> object
                            (map (fn [[attribute value]]
                                   [attribute
                                    (if-let [{:keys [to-many uuids]} (get-in queries-result [(:app/uuid object) attribute])]
                                      (if to-many
                                        {:add [] ;; value is nil
                                         :remove (map #(select-keys % [:app/uuid]) uuids)}
                                        {:add (vec (keep identity [value]))
                                         :remove (vec (keep identity (let [[db-value] uuids]
                                                                       [(when (not= value db-value)
                                                                          db-value)])))})
                                      value)]))
                            (into {}))])
             flat-parsed-input)

        new-objects (mapcat
                     (fn [[_ attrs]]
                       (mapcat
                        (fn [[_ {:keys [entity uuids]}]]
                          (map #(vector entity %) uuids))
                        attrs))
                     queries-result)]
    (concat updated-objects new-objects)))

(defn collect-input-types
  "Check if the same UUID does not represent different types in input.

   Input: entities-by-name, flat-parsed-input.

   Output: map {uuid -> entity name} or failjure error in case of contradiction."
  [{:keys [entities-by-name]} flat-parsed-input]
  (->> (mapcat #(apply expected-types entities-by-name %) flat-parsed-input)
       (group-by first)
       (u/reduce-failures
        (fn [type-map [uuid values]]
          (let [types-set (set (map second values))]
            (if (> (count types-set) 1)
              (f/fail "Value with id %s is expected to have types %s at the same time."
                      uuid (str/join ", " types-set))
              (assoc type-map uuid (first types-set)))))
        {})))

(defn get-db-types
  "Input: db, list of uuids.

   Output: map {uuid -> db-type} for these uuids which exist in the database."
  [{:keys [q]} db uuids]
  (when (seq uuids)
    (->> (q '[:find (pull ?id [*])
              :in $ [?uuid ...]
              :where [?id :app/uuid ?uuid]] db uuids)
         (map (fn [[db-entity]]
                (->> db-entity
                     (keep (fn [[k]] (when (= (name k) "ident__") (namespace k))))
                     first
                     (vector (:app/uuid db-entity)))))
         (into {}))))

(defn compare-types
  "Check if two maps with input types and db types are not contradicting each other."
  [input-types db-types]
  (u/fail-many (keep (fn [[uuid db-type]]
                       (let [input-type (get input-types uuid)]
                         (when (not= db-type input-type)
                           (str "Value with id " uuid " has input type " input-type
                                ", but in database that object has type " db-type "."))))
                     db-types)))

(defn validate-values
  "Check if values conform to specification in schema (min, max, regex)."
  [{:keys [entities-by-name]} flat-parsed-input]
  (->> flat-parsed-input
       (mapcat (fn [[entity object]]
                 (mapcat #(apply validate-value entities-by-name entity (:app/uuid object) %) object)))
       flatten
       u/fail-many))

(defn merge-input
  "Previously used flat-parsed-input might still contain multiple elements describing one object.
   Try to merge them.

   Input: flat-parsed-input.

   Output: updated flat-parsed-input (with merged elements) or failjure error in case of contradiction."
  [flat-parsed-input]
  (let [flat-parsed-input
        (->> flat-parsed-input
             (group-by (comp :app/uuid second))
             (map (fn [[_uuid inputs]]
                    (let [maps (map second inputs)
                          merged-map (apply merge-with merge-values maps)
                          verified-map (->> merged-map
                                            (map (fn [[k v]] [k (cond-> v (add-remove-map? v) add-and-remove)]))
                                            (into {}))]
                      [(ffirst inputs) verified-map]))))

        contradiction-errors
        (->> flat-parsed-input
             (keep (fn [[entity object]]
                     (let [contradictions
                           (keep (fn [[k v]]
                                   (when (f/failed? v) (str (name k) ": " (:message v))))
                                 object)]
                       (when (seq contradictions)
                         (str "Contraditions in " entity " (uuid: " (:app/uuid object) "):\n"
                              (str/join ",\n" contradictions))))))
             u/fail-many)]
    (if (f/failed? contradiction-errors)
      contradiction-errors
      flat-parsed-input)))

(defn validate-obligatory-attributes
  "Check if mutation tries to erase obligatory field from existing object or create new without required values.

   Input: entities-by-name, flat-parsed-input,
          new? - function which checks if object with given uuid already exists in database."
  [{:keys [entities-by-name default-values]} flat-parsed-input new?]
  (->> flat-parsed-input
       (mapcat (fn [[entity object]]
                 (let [required-fields (->> (get entities-by-name entity)
                                            :attributes
                                            (keep (fn [[attribute {:keys [optional]}]]
                                                    (when (and (not ((model/special-attributes true) attribute))
                                                               (not optional)
                                                               (not (#{"uuid" "active"} attribute)))
                                                      (keyword entity attribute)))))
                       uuid (:app/uuid object)]
                   (if (new? uuid)
                     (map (fn [attribute]
                            (when (if (contains? object attribute)
                                    (nil? (attribute object)) ;; present and nil
                                    (nil? (get-in default-values [entity attribute]))) ;; missing and does not have default value
                              (str attribute " (uuid: " uuid "): value is missing.")))
                          required-fields)
                     (map (fn [attribute]
                            (when (and (contains? object attribute)
                                       (nil? (attribute object))) ;; present and nil
                              (str attribute " (uuid: " uuid "): value can't be deleted.")))
                          required-fields)))))
       flatten
       u/fail-many))

(defn custom-verifiers-grouped
  [custom-field-verifiers]
  (->> custom-field-verifiers
       (group-by (comp namespace first))
       (map (fn [[entity verifiers]] [entity (into {} verifiers)]))
       (into {})))

(defn verify-one-object
  [{:keys [entities-by-name custom-field-verifiers]} entity object]
  (let [verifiers (get (custom-verifiers-grouped custom-field-verifiers) entity)
        errors (keep (fn [[field verifier]]
                       (when (contains? object field)
                         (when-let [error (verifier {:value (field object)
                                                     :entities-by-name entities-by-name
                                                     :object object})]
                           [(name field) {:value (field object)
                                          :error error}])))
                     verifiers)]
    (some->> errors seq (into {}) (hash-map (:app/uuid object)))))

(defn check-object-custom-verifiers
  [config flat-parsed-input]
  (let [errors-map (->> flat-parsed-input
                        (map (partial apply verify-one-object config))
                        (apply merge))]
    (when (seq errors-map)
      (u/fail "Individual fields failed verification." {"objectFieldsVerification" errors-map}))))

(defn create-update-datoms
  [{:keys [entities-by-name]} ->n-identifier now [entity object]]
  (let [uuid (:app/uuid object)
        entity-description (get entities-by-name entity)]
    (->> (dissoc object :app/uuid)
         (reduce (fn [result [attribute value]]
                   (if (nil? value)
                     ;; Delete value
                     (if-let [attribute-description (get-in entity-description [:attributes (name attribute)])]
                       (if (:custom-input attribute-description)
                         result
                         (add-datom result [:db/retract [:app/uuid uuid] attribute]))
                       (assert false "All nils in relationships shoud be replaced"))
                     ;; Update value
                     (if-let [attribute-description (get-in entity-description [:attributes (name attribute)])]
                       (if (:custom-input attribute-description)
                         result
                         (add-datom result [:db/add [:app/uuid uuid] attribute value]))
                       (if (get-in entity-description [:datomic-relationships (name attribute)])
                         (let [adds (map (fn [inverse-uuid]
                                           [:db/add [:app/uuid uuid] attribute (->n-identifier inverse-uuid)])
                                         (:add value))
                               retracts (map (fn [inverse-uuid]
                                               [:db/retract [:app/uuid uuid] attribute (->n-identifier inverse-uuid)])
                                             (:remove value))
                               datoms (concat adds retracts)]
                           (reduce add-datom (add-n-identifiers result (map last datoms)) datoms))
                         (let [{:keys [inverse-entity inverse-name]}
                               (get-in entity-description [:relationships (name attribute)])
                               backward-attr (keyword inverse-entity inverse-name)
                               adds (map (fn [inverse-uuid]
                                           [:db/add (->n-identifier inverse-uuid) backward-attr [:app/uuid uuid]])
                                         (:add value))
                               retracts (map (fn [inverse-uuid]
                                               [:db/retract (->n-identifier inverse-uuid) backward-attr [:app/uuid uuid]])
                                             (:remove value))
                               datoms (concat adds retracts)]
                           (reduce add-datom (add-n-identifiers result [[:app/uuid uuid]]) datoms))))))
                 [#{} []])
         (->datoms-only now))))

(defn create-add-datoms
  [{:keys [entities-by-name]} ->n-identifier now [entity object]]
  (let [uuid (:app/uuid object)
        entity-description (get entities-by-name entity)

        {:keys [attributes datomic-relationships relationships]}
        (->> (dissoc object :app/uuid)
             (remove (comp nil? second))
             (group-by (fn [[attribute]]
                         (->> [:attributes :datomic-relationships :relationships]
                              (filter #(get-in entity-description [% (name attribute)]))
                              first))))

        attributes-datoms
        (map (fn [[attribute value]] [:db/add (str uuid) attribute value]) attributes)

        datomic-relationships-datoms
        (->> datomic-relationships
             (reduce (fn [result [attribute value]]
                       (let [adds (map (fn [inverse-uuid] [:db/add (str uuid) attribute (->n-identifier inverse-uuid)])
                                       (:add value))]
                         (reduce add-datom (add-n-identifiers result (map last adds)) adds)))
                     [#{} []])
             (->datoms-only now))

        relationships-datoms
        (->> relationships
             (reduce (fn [result [attribute value]]
                       (let [{:keys [inverse-entity inverse-name]}
                             (get-in entity-description [:relationships (name attribute)])
                             backward-attr (keyword inverse-entity inverse-name)
                             adds (map (fn [inverse-uuid] [:db/add (->n-identifier inverse-uuid) backward-attr (str uuid)])
                                       (:add value))]
                         (reduce add-datom (add-n-identifiers result [(str uuid)]) adds)))
                     [#{} []])
             (->datoms-only now))]
    (concat attributes-datoms datomic-relationships-datoms relationships-datoms)))

(defn check-db-custom-verifier
  [{:keys [entities-by-name custom-db-verifiers]} flat-parsed-input initial-db speculation-db types-info-map mock? context user-db-id]
  (let [errors-map (->> custom-db-verifiers
                        (keep (fn [[verifier-name verifier-fn]]
                                (when-let [error (verifier-fn {:entities-by-name entities-by-name
                                                               :flat-parsed-input flat-parsed-input
                                                               :initial-db initial-db
                                                               :db speculation-db
                                                               :types-info-map types-info-map
                                                               :mock? mock?
                                                               :context context
                                                               :user-db-id user-db-id})]
                                  {verifier-name error})))
                        (apply merge))]
    (when (seq errors-map)
      (u/fail "Mutation violates database invariants" {"databaseInvariantsVerification" errors-map}))))

(defn update-with-custom-transaction-modifiers
  [{:keys [entities-by-name with custom-transaction-modifiers]} flat-parsed-input transaction initial-db speculation-db now types-info-map context]
  (reduce (fn [state [_modifier-name modifier-fn]]
            (let [modifier-result (modifier-fn state)]
              (if (f/failed? modifier-result)
                (reduced modifier-result)
                (let [{:keys [transaction]} modifier-result]
                  (merge (assoc state
                                :transaction transaction
                                :db (:db-after (with (:initial-db state) transaction)))
                         modifier-result)))))
          {:transaction transaction
           :entities-by-name entities-by-name
           :flat-parsed-input flat-parsed-input
           :initial-db initial-db
           :db speculation-db
           :now now
           :types-info-map types-info-map
           :context context}
          custom-transaction-modifiers))

(defn create-upsert
  [{:keys [with log-mutation transact conn user-rules-by-id] :as config} entity mock?]
  (fn [context args _value]
    (let [log-data (atom {})
          log! (fn [ks v] (swap! log-data assoc-in ks v))
          now (u/now)]
      (f/attempt-all
       [data ((keyword (:name entity)) args)
        user-db-id (get-in context [:request :lacinia-app-context :user :db/id])
        parsed-input (mapcat #(parse-input % entity config) data)
        db (get-in context [:request :lacinia-app-context :database])
        _ (log! [:db :initial-db] db)
        user-rules (user-rules-by-id context user-db-id db)
        user-scoped-ids (scope/scoped-ids config context user-db-id db user-rules)
        flat-input (mapcat (fn [[entity values]]
                             (mapv vector (repeat entity) values))
                           parsed-input)
        flat-parsed-input (parse-flat-input config flat-input)
        flat-parsed-input (add-detached-objects config flat-parsed-input db user-scoped-ids)
        input-types (collect-input-types config flat-parsed-input)
        db-types (get-db-types config db (keys input-types))
        new? #(cond->> %
                (sequential? %) second
                (not (uuid? %)) :app/uuid
                true (contains? db-types)
                true not)
        _db-type-errors (compare-types input-types db-types)
        types-info-map (merge input-types db-types)
        _value-errors (validate-values config flat-parsed-input)
        flat-parsed-input (merge-input flat-parsed-input)
        _optionality-errors (validate-obligatory-attributes config flat-parsed-input new?)
        _ (log! [:flat-parsed-input] flat-parsed-input)
        ;; Current behavior is: when updating to-many relationship of either new or existing object,
        ;; we don't check if the relation existed.
        _scoping-errors (scope/validate-scoping-before-transaction config
                                                                   context
                                                                   flat-parsed-input
                                                                   user-db-id
                                                                   db
                                                                   user-scoped-ids
                                                                   new?)
        _custom-object-verification-errors (check-object-custom-verifiers config flat-parsed-input)
        new-datoms (->> flat-parsed-input
                        (filter new?)
                        (map (fn [[entity object]]
                               (let [tempid (str (:app/uuid object))] ;; introduce tempid
                                 (u/new-entity config entity now (assoc object :db/id tempid))))))
        ->n-identifier (fn [uuid]
                         (let [uuid (cond (map? uuid) (:app/uuid uuid)
                                          (sequential? uuid) (second uuid)
                                          :else uuid)]
                           (if (new? uuid)
                             (str uuid) ;; tempid
                             [:app/uuid uuid]))) ;; identifier -> lookup-ref
        updates (->> flat-parsed-input
                     (remove new?)
                     (mapcat (partial create-update-datoms config ->n-identifier now)))
        adds (->> flat-parsed-input
                  (filter new?)
                  (mapcat (partial create-add-datoms config ->n-identifier now)))
        transaction (concat new-datoms updates adds)
        _ (log! [:transaction :initial] transaction)
        with-db (try (with db transaction)
                     (catch Exception e
                       (log! [:exception] e)
                       (f/fail "Unknown exception on applying initial changes to database!")))
        {:keys [db-before db-after]} with-db
        user-scoped-ids-after (scope/scoped-ids config context user-db-id db-after user-rules)
        _ (log! [:db :with-db] (dissoc with-db :tempids :tx-data))
        _scoping-errors (scope/validate-scoping-after-transaction config flat-parsed-input user-scoped-ids-after db-after)
        _obligatory-relations-errors (relations/validate-obligatory-relations config
                                                                              flat-parsed-input
                                                                              user-scoped-ids-after
                                                                              db-after)
        _custom-db-verification-errors (check-db-custom-verifier config
                                                                 flat-parsed-input
                                                                 db-before db-after
                                                                 types-info-map
                                                                 mock?
                                                                 context
                                                                 user-db-id) ;; zmiana arg!
        transaction-with-custom-modifiers (try (update-with-custom-transaction-modifiers config
                                                                                         flat-parsed-input
                                                                                         transaction
                                                                                         db-before
                                                                                         db-after
                                                                                         now
                                                                                         types-info-map
                                                                                         context)
                                               (catch Exception e
                                                 (log! [:exception] e)
                                                 (f/fail "Unknown exception on applying additional changes to database!")))
        {transaction :transaction mock-db :db} transaction-with-custom-modifiers
        transaction (into transaction (log/audit-datoms config context now))
        _ (log! [:transaction :final] transaction)
        seq-pull-structure (patterns/sequential-pull-structure context config)
        final-db (if mock?
                   mock-db
                   (try (let [transact-result (transact conn transaction)]
                          (log! [:db :final-db] (dissoc transact-result :tempids :tx-data))
                          (:db-after transact-result))
                        (catch Exception e
                          (log! [:exception] e)
                          (f/fail "Unknown exception on applying all changes to database!"))))]
       (do (when log-mutation (log-mutation context args mock? nil now @log-data))
           (pull/pull-by-uuids config context final-db (map :uuid data) user-db-id seq-pull-structure))
       (f/when-failed [e] (do (when log-mutation (log-mutation context args mock? e now @log-data))
                              (u/failure->lacinia-error config e)))))))
