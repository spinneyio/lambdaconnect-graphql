(ns lambdaconnect-graphql.sort 
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [failjure.core :as f]
            [lambdaconnect-model.core :as lm]
            [lambdaconnect-graphql.utils :as u]))

;; Relation output object:
;; :entity-name
;; :state - :entity, :entities, :value, :values or :agg-value
;; :agg-symbol - aggregate function (non-nil only if state is :agg-value)
;; :datomic-clauses - to be used in generated rule
;; :var-ctr - used to generate fresh variables

;; States:
;; :entity INITIAL
;; --attribute--> :value
;; --to-one-rel-> :entity
;; -to-many-rel-> :entities
;; :value FINAL (for single-value filters and sorts, including custom attributes) (no transitions)
;; :entities
;; ---to-*-rel--> :entities
;; --attribute--> :values
;; :values FINAL (for multi-value filters, excluding custom attributes)
;; --aggregate--> :agg-value
;; :agg-value FINAL (for single-value filters and sorts, excluding custom attributes) (no transitions)

(def initial-state :entity)
(def single-value-final-states #{:value :agg-value})
(def multiple-values-final-states #{:values})
(def descriptive-states
  {:entity "a single entity"
   :value "a single value"
   :entities "a list of entities"
   :values "a list of values"
   :agg-value "an aggregate value"})

(defn ?var
  [no]
  (symbol (str "?var" no)))

(def legal-aggregates
  "All aggregates built in Datomic that return only one value and work for all types.
   Notice that, to keep up with attributes/relations naming conventions,
   we have \"countDistinct\" and not \"count-distinct\"."
  {"countDistinct" {:fn #(->> % (map (fn [[id values]] [id (count values)])) (into {}))
                    :value-type :db.type/long}
   ;; use reduce / compare to find max of any comparable type, not just nums
   "max" {:fn #(->> % (map (fn [[id values]] [id (some->> values seq (reduce (fn [x y] (if (>= (compare x y) 0) x y))))])))}
   "min" {:fn #(->> % (map (fn [[id values]] [id (some->> values seq (reduce (fn [x y] (if (<= (compare x y) 0) x y))))])))}})

(defn single-step-standard-fn
  [{:keys [q]} {:keys [db scoped-ids]} entity-name entity-description relation transition-info state transition]
  (fn [ids->values]
    (let [forward-attr (when (or (get-in entity-description [:attributes relation])
                                 (get-in entity-description [:datomic-relationships relation]))
                         (keyword (if (lm/special-attribs relation) "app" entity-name) relation))
          {:keys [inverse-entity inverse-name]} (get-in entity-description [:relationships relation])
          scope (when (= transition :relationship) ;; Run scoping query in parallel with step query
                  (future @(scoped-ids inverse-entity)))
          attr (or forward-attr (keyword inverse-entity inverse-name))
          is-relationship? (not (get-in entity-description [:attributes relation]))
          var0 (?var 0)
          var1 (?var 1)
          relation-rule (if forward-attr [var0 attr var1] [var1 attr var0])
          query (vec (concat [:find var0 (cond->> var1 (:to-many transition-info) (list 'set))
                              :in '$ [var0 '...]
                              :where relation-rule]
                             (when is-relationship? [[var1 :app/active true]])))
          input-values (-> ids->values vals set u/union)
          response (->> input-values
                        (q query db)
                        (into {}))
          scoped-response (case transition
                            :relationship (->> response
                                               (map (fn [[id value-s]]
                                                      [id (if (:to-many transition-info)
                                                            (set/intersection value-s @scope) ;; set -> set (may be empty)
                                                            (@scope value-s))])) ;; scalar value -> scalar value or nil
                                               (into {}))
                            :attribute response)]
      (->> ids->values
           (map (fn [[id value-s]] [id (case state
                                         :entity (scoped-response value-s)
                                         :entities (->> value-s (map scoped-response) u/union))]))
           (into {})))))

(defn single-step-custom-fn
  [{:keys [custom-attributes custom-relationships]} {:keys [_db _ids _scoped-ids] :as args} entity-name relation state transition]
  (fn [ids->values]
    (let [attr (keyword entity-name relation)
          custom-fn (case transition
                      :custom-relationship (-> custom-relationships attr :fn)
                      :custom-attribute (-> custom-attributes attr :fn))
          scoped-response (->> ids->values vals u/union (assoc args :ids) custom-fn)]
      (->> ids->values
           (map (fn [[id value-s]] [id (case state
                                         :entity (scoped-response value-s)
                                         :entities (->> value-s (map scoped-response) u/union))]))
           (into {})))))

(defn parse-nested-relation-single-fn
  [[single-relation & rest-relation]
   {:keys [entities-by-name custom-attributes custom-relationships] :as config}
   {:keys [untokenized-text-relation
           entity-name
           state
           fn-closure
           result-fn
           value-type
           final-states]
    :as step-data}]
  (let [error (fn [& args] (f/fail (apply str "Error when parsing relationship \""
                                          untokenized-text-relation "\":\n" args)))]
    (if single-relation
      (let [error (fn [& args] (apply error "at token \"" single-relation "\":\n" args))]
        (case state
          (:entity :entities)
          (f/attempt-all
           [entity-description (get entities-by-name entity-name)
            
            parsed-relation
            (or (when-let [attribute (get-in entity-description [:attributes single-relation])]
                  [:attribute attribute])
                (when-let [relation (get-in entity-description [:relationships single-relation])]
                  [:relationship relation])
                (when-let [custom-attribute (get custom-attributes (keyword entity-name single-relation))]
                  [:custom-attribute custom-attribute])
                (when-let [custom-relation (get custom-relationships (keyword entity-name single-relation))]
                  [:custom-relationship custom-relation])
                (error "Expected an attribute or a relation."))

            [transition transition-info] parsed-relation

            next-state
            (if (#{:relationship :custom-relationship} transition)
              (if (or (= state :entities) (:to-many transition-info))
                :entities
                :entity)
              (if (= state :entities)
                :values
                :value))

            ;; ids->values is map with ids as keys and values dependent on current state:
            ;; :entity -> db/id (or nil)
            ;; :entities -> set of db/id (may be empty, may not be nil)
            ;; :value or :agg-value -> scalar value (or nil)
            ;; :values -> set of scalar values (may be empty, may not be nil)
            ;; Important: keys should not disappear from map!
            step-fn (if (#{:custom-attribute :custom-relationship} transition)
                      (single-step-custom-fn config fn-closure entity-name single-relation state transition)
                      (single-step-standard-fn config fn-closure entity-name entity-description single-relation transition-info state transition))]
           (recur
            rest-relation
            config
            (assoc step-data
                   :state next-state
                   :value-type (:type transition-info) ;; nil in case of (custom) relation
                   :entity-name (or (:inverse-entity transition-info) ;; nil in case of (custom) attribute
                                    entity-name)
                   :result-fn (comp step-fn result-fn))))

          :values
          (if-let [agg-description (legal-aggregates single-relation)]
            (recur
             rest-relation
             config
             (assoc step-data
                    :state :agg-value
                    :value-type (or (:value-type agg-description) value-type)
                    :entity-name nil
                    :result-fn (comp (:fn agg-description) result-fn)))
            (error "Expected one of the aggregate functions: " (str/join ", " (keys legal-aggregates))))

          (error "No further tokens expected after " (descriptive-states state) ".")))
      (if (final-states state)
        {:result-fn result-fn
         :value-type value-type}
        (error "Expected " (str/join " or " (map descriptive-states final-states))
               " at the end, got " (descriptive-states state) " instead.")))))

(defn nested-relation->fn
  "Parse complex attributes that can appear in sort/filter queries.
   Output: failjure error OR map with keys:
   * :result-fn - function which returns collection of ids and returns map {id -> value-s},
   * :agg-description - optional
   * :value-type - type of values in result map of result-fn"
  [config entity-name text-relation single-final-value? db scoped-ids rules]
  (parse-nested-relation-single-fn
   (str/split text-relation #"\.")
   config
   {:untokenized-text-relation text-relation
    :entity-name entity-name
    :state initial-state
    :fn-closure {:db db :scoped-ids scoped-ids :rules rules}
    :result-fn (fn [ids] (->> ids (map (fn [id] [id id])) (into {})))
    :final-states (if single-final-value?
                    single-value-final-states
                    multiple-values-final-states)}))

(defn execute-sort-subquery
  "Function returns map {id -> [sort value]}. Sort value is in vector to make it easier
   to concat values in case of multiple sorts."
  [parsed-sort ids]
  (->> ((:result-fn parsed-sort) ids)
       (map (fn [[value sort-value]] [value [sort-value]]))
       (into {})))

(def comparator-combinators
  [[:CASE_INSENSITIVE
    (fn [comparator]
      (let [to-lower (fn [x] (cond-> x (string? x) str/lower-case))]
        (fn [x y] (comparator (to-lower x) (to-lower y)))))]
   [:DESC_ORDER
    (fn [comparator]
      (fn [x y] (- (comparator x y))))]
   [:NULLS_FIRST
    (fn [comparator]
      (fn [x y] (if (nil? x)
                  (if (nil? y) 0 -1)
                  (if (nil? y) 1 (comparator x y)))))]
   [:NULLS_LAST
    (fn [comparator]
      (fn [x y] (if (nil? x)
                  (if (nil? y) 0 1)
                  (if (nil? y) -1 (comparator x y)))))]])

(defn comparator-by-coordinates
  "Assumption: comparators, xs and ys all have the same length."
  [comparators xs ys]
  (if (empty? comparators)
    0
    (let [[x & xs] xs
          [y & ys] ys
          [custom-comparator & comparators] comparators
          compare-value (custom-comparator x y)]
      (if (zero? compare-value)
        (recur comparators xs ys)
        compare-value))))

(def forbidden-options-combinations
  [[#{:NULLS_FIRST :NULLS_LAST}
    "Flags NULLS_FIRST and NULLS_LAST were used at the same time."]])

(defn check-forbidden-options-combinations
  [options]
  (u/fail-many (keep (fn [[forbidden message]]
                       (when (set/subset? forbidden options)
                         message))
                     forbidden-options-combinations)))
