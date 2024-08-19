(ns lambdaconnect-graphql.model
  (:require [clojure.string :as str]
            [lambdaconnect-model.core :as lm]
            [lambdaconnect-graphql.utils :as u]
            [lambdaconnect-graphql.types :as types]))

(def query-special-attributes #{"syncRevision"})
(def mutation-special-attributes #{"createdAt" "updatedAt"})
(defn special-attributes [input?]
  (cond-> query-special-attributes input? (into mutation-special-attributes)))

(defn ->non-null [t] `(~'non-null ~t))
(defn ->list [t] `(~'list ~t))



(def common-descriptions
  {"active" "If you create a new object or edit an existing one, you can set this attribute to \"true\" or leave it empty.
             If you want to delete an object, set this attribute to \"false\"."})

(defn attribute->graphql
  [{:keys [entity-name name type optional user-info default-value]} input? deprecated-fields]
  (let [t (type types/types-map)]
    (assert t "Attribute is not a basic type")
    {(keyword name)
     (merge {:type (cond-> t
                     (if input? (= name "uuid") (not optional)) ->non-null)}
            (let [common-doc (get common-descriptions name)
                  xml-doc (get user-info "docs")
                  default-value-doc (when (some? default-value)
                                      (str "Default value is " default-value "."))
                  doc (str/join "\n\n" (keep identity [common-doc xml-doc default-value-doc]))]
              (when (seq doc) {:description doc}))
            (when-let [reason (get deprecated-fields (keyword entity-name name))]
              {:deprecated reason}))}))

(defn relationship->graphql
  [{:keys [destination-entity entity-name name to-many optional user-info]} input? deprecated-fields]
  (let [destination (keyword (str destination-entity (when input? "_input")))]
    {(keyword name)
     (merge {:type (if (and input? to-many)
                     (keyword (str destination-entity "_relation_input"))
                     (cond-> destination
                       (and (not input?) (not optional)) ->non-null
                       to-many ->list))}
            (when-not input?
              {:resolve (keyword entity-name name)})
            (let [common-doc (get common-descriptions name)
                  xml-doc (get user-info "docs")
                  doc (str/join "\n\n" (keep identity [common-doc xml-doc]))]
              (when (seq doc) {:description doc}))
            (when-let [reason (get deprecated-fields (keyword entity-name name))]
              {:deprecated reason}))}))

(defn entity->graphql-object
  [{:keys [name attributes relationships user-info]} input? deprecated-fields]
  {(keyword (str name (when input? "_input")))
   (merge {:fields
           (apply merge (concat (map #(attribute->graphql % input? deprecated-fields)
                                     (remove (comp (special-attributes input?) :name)
                                             (vals attributes)))
                                (map #(relationship->graphql % input? deprecated-fields)
                                     (vals relationships))))}
          (let [xml-doc (get user-info "docs")
                doc (str/join "\n\n" (keep identity [xml-doc]))]
            (when (seq doc) {:description doc}))
          (when-let [doc (get user-info "docs")]
            {:description doc}))})

(defn entity->graphql-list-output-object
  [{:keys [name]}]
  {(keyword (str name "_list_output"))
   {:fields {:value {:type (->list (keyword name))}
             :count {:type 'Int}}}})

(defn entity->graphql-relation-input
  [{:keys [name user-info]}]
  (let [input (-> name (str "_input") keyword ->non-null ->list)]
    {(keyword (str name "_relation_input"))
     (merge {:fields
             {:add {:type input
                    :description "Objects to be added to the relationship"}
              :remove {:type input
                       :description "Objects to be removed from the relationship"}}}
            (when-let [doc (get user-info "docs")]
              {:description doc}))}))

(defn entity->graphql-queries
  [{:keys [name]}]
  {(keyword (str name "_all"))
   {:type (keyword (str name "_list_output"))
    :args {:sort {:type (-> :Sort ->non-null ->list)}
           :pagination {:type :Pagination}
           :filter {:type :FilterAnd}}
    :resolve (keyword "query" (str name "-all"))}
   (keyword (str name "_by_uuid"))
   {:type (->list (keyword name))
    :args '{:uuid {:type (list (non-null ID))}}
    :resolve (keyword "query" (str name "-by-uuid"))}})

(defn entity->graphql-create-mutation
  [{:keys [name]} mock?]
  (let [input (-> name (str "_input") keyword ->non-null ->list)]
    {(keyword (str (when mock? "mock_") "upsert_" name))
     {:resolve (keyword "mutation" (str (when mock? "mock-") "upsert-" name))
      :type (->list (keyword name))
      :args {(keyword name) {:type input}}}}))

(def basic-custom-schema
  '{:enums
    {:SortingOption
     {:values [{:enum-value :CASE_INSENSITIVE
                :description "If sorting by strings, use case insensitive values. Otherwise, this option is ignored."}
               {:enum-value :DESC_ORDER
                :description "Sort query results by descending order (default is ascending order)."}
               {:enum-value :NULLS_FIRST
                :description "Show objects where sorting value is null first, regardless of sorting order. Exception will be thrown if used together with NULLS_LAST."}
               {:enum-value :NULLS_LAST
                :description "Show objects where sorting value is null last, regardless of sorting order. Exception will be thrown if used together with NULLS_FIRST."}]}}
  
    :directive-defs
    {:filterSortPaginate {:locations #{:object}
                          :args {:filter {:type FilterAnd}
                                 :sort {:type (list (non-null Sort))}
                                 :pagination {:type Pagination}}}}
  
    :input-objects
    {:Pagination {:description "Allows for paginating the query"
                  :fields
                  {:page {:type (non-null Int)}
                   :perPage {:type (non-null Int)}}}
     :Sort {:description "Allows for sorting the query results"
            :fields
            {:field {:type (non-null String)}
             :options {:type (list (non-null :SortingOption))}}}
     :Filter {:description "Allows for filtering the query result"
              :fields
              {:negate {:type Boolean}
               :field {:type (non-null String)}
               :comparator {:type (non-null String)
                            :description "Possible values are: <=, <, >=, >, =, !=, isnull, notnull, contains, regex"}
               :value {:type String
                       :description "Obligatority depends on comparator value"}}}
     :FilterOr {:description "At least one filter must be satisfied"
                :fields
                {:or {:type (list (non-null :Filter))}}}
     :FilterAnd {:description "All filters must be satisfied"
                 :fields
                 {:and {:type (list (non-null :FilterOr))}}}}
  
    :objects {}
    :mutations {}})

(defn custom-schema
  [custom-input-object-fields custom-object-fields]
  (u/deep-merge basic-custom-schema {:input-objects custom-input-object-fields
                                     :objects custom-object-fields}))

(defn model
  [entities-by-name custom-schema deprecated-fields]
  (let [model-description {:objects (apply merge (concat (map #(entity->graphql-object % false deprecated-fields) (vals entities-by-name))
                                                         (map entity->graphql-list-output-object (vals entities-by-name))))
                           :queries (apply merge (map entity->graphql-queries (vals entities-by-name)))
                           :mutations (apply merge (concat (map #(entity->graphql-create-mutation % true) (vals entities-by-name))
                                                           (map #(entity->graphql-create-mutation % false) (vals entities-by-name))))
                           :input-objects (apply merge (concat (map #(entity->graphql-object % true deprecated-fields) (vals entities-by-name))
                                                               (map entity->graphql-relation-input (vals entities-by-name))))}]
    (u/deep-merge model-description custom-schema)))

(defn- find-resolvers
  [m]
  (if (map? m)
    (mapcat find-resolvers m)
    (let [[k v] m]
      (cond (= k :resolve) [v]
            (map? v) (mapcat find-resolvers v)))))

(defn custom-resolvers
  [custom-schema]
  (set (find-resolvers custom-schema)))

(defn entities-by-name-with-custom-input
  [entities-by-name custom-schema]
  (let [custom-input-attributes
        (->> custom-schema
             :input-objects
             (keep (fn [[entity-input {:keys [fields]}]]
                     (let [[_ entity] (re-matches #"(.*)_input" (name entity-input))]
                       (when entity
                         [entity {:attributes (->> fields
                                                   (map (fn [[field m]]
                                                          [(name field)
                                                           {:custom-input true
                                                            :name (name field)
                                                            :entity-name entity
                                                            :type (get types/inverse-types-map (:type m))
                                                            :optional true}]))
                                                   (into {}))}]))))
             (into {}))]
    (u/deep-merge entities-by-name custom-input-attributes)))

(defn default-values
  [entities-by-name]
  (->> entities-by-name
       vals
       (map (fn [{:keys [name attributes]}]
              [name (->> attributes
                         (keep (fn [[attr-name {:keys [default-value]}]]
                                 (when (some? default-value)
                                   [(keyword (if (lm/special-attribs attr-name) "app" name)
                                             attr-name)
                                    default-value])))
                         (into {}))]))
       (into {})))
