(ns lambdaconnect-graphql.core 
  (:require [clojure.spec.alpha :as s]
            [lambdaconnect-model.core :as lm]
            [lambdaconnect-graphql.interceptors :as interceptors]
            [lambdaconnect-graphql.model :as model]
            [lambdaconnect-graphql.resolvers :as resolvers]
            [lambdaconnect-graphql.spec :as spec]
            [lambdaconnect-graphql.constants :as constants]))

(def empty-value
  "Often, when a Datomic query returns an attribute for an entity, that attribute might not exist.
   Using nil in Datomic often leads to erros, since Datomic would skip the row.
   Instead, use this value as a placeholder for nil in Datomic queries and library will manage it properly."
  constants/empty-value)

(defn custom-schema
  "Create a custom part of a GraphQL map describing a model.
   Each of the inputs is a nested map {entity-name -> {field-name -> {type, description}}}.
   In custom-input-object-fields, entity-name must have suffix _input."
  [custom-input-object-fields custom-object-fields]
  (model/custom-schema custom-input-object-fields custom-object-fields))

(s/fdef custom-schema
  :args (s/and (s/cat :custom-input-object-fields ::spec/custom-input-object-fields
                      :custom-object-fields ::spec/custom-object-fields)))

(defn graphql-model
  "Create a GraphQL map describing a model.
   Input:
   * entities-by-name
   * custom-schema (result of custom-schema function)
   * deprecated-fields: map {namespaced key :Entity/attribute -> string description of why the field is deprecated}
     In the case of input objects deprecation the implementation is ready and waiting for GraphQL RFC to proceed."
  [entities-by-name custom-schema deprecated-fields]
  (model/model entities-by-name custom-schema deprecated-fields))

(s/fdef graphql-model
  :args (s/cat :deprecated-fields ::spec/deprecated-fields))

(defn lambdaconnect-graphql-config
  [{:keys [entities-by-name datomic-schema custom-schema
           conn q pull db with-db with transact
           interceptor
           resolve-as parsed-query-key selection-key selection-kind
           answer-key-fns
           custom-field-verifiers
           custom-db-verifiers
           custom-transaction-modifiers
           custom-attributes
           custom-relationships
           log-mutation
           user-rules-by-id
           force-sequential-pull-execution?
           get-scoping-permissions]}]
  (let [default-values (model/default-values entities-by-name)
        datomic-attrs (->> datomic-schema (map (juxt :db/ident identity)) (into {}))
        datomic-attrs-only (->> datomic-attrs (filter (fn [[_ v]] (not= (:db/valueType v) :db.type/ref))) (into {}))
        datomic-relationships-only (->> datomic-attrs (filter (fn [[_ v]] (= (:db/valueType v) :db.type/ref))) (into {}))
        custom-resolvers (model/custom-resolvers custom-schema)]
    {:entities-by-name entities-by-name
     :custom-schema custom-schema
     :conn conn
     :q q
     :pull pull
     :db db
     :with-db with-db
     :with with
     :transact transact
     :interceptor interceptor
     :resolve-as resolve-as
     :parsed-query-key parsed-query-key
     :selection-key selection-key
     :selection-kind selection-kind
     :answer-key-fns answer-key-fns
     :custom-field-verifiers custom-field-verifiers
     :custom-db-verifiers custom-db-verifiers
     :custom-transaction-modifiers custom-transaction-modifiers
     :custom-attributes custom-attributes
     :custom-relationships custom-relationships
     :user-rules-by-id user-rules-by-id
     :force-sequential-pull-execution? force-sequential-pull-execution?
     :get-scoping-permissions get-scoping-permissions
     
     :log-mutation (or log-mutation (constantly nil))
     
     :custom-resolvers custom-resolvers
     :default-values default-values
     :datomic-attrs datomic-attrs
     :datomic-attrs-only datomic-attrs-only
     :datomic-relationships-only datomic-relationships-only}))

(s/fdef lambdaconnect-graphql-config
  :ret ::spec/config)

(defn resolvers
  "Define all queries and mutations associated with the schema."
  [config]
  (resolvers/resolvers config))

(s/fdef resolvers
  :args (s/cat :config ::spec/config))

;; Interceptors

(defn db-grapqhl-interceptor
  "Interceptor that adds snapshot of the database to the Lacinia context, required."
  [config]
  (interceptors/db-grapqhl-interceptor config))

(s/fdef db-grapqhl-interceptor
  :args (s/cat :config ::spec/config))

(defn user-graphql-interceptor
  "Interceptor that adds pulled user to the Lacinia context (requires db-grapqhl-interceptor first), required."
  [config]
  (interceptors/user-graphql-interceptor config))

(s/fdef user-graphql-interceptor
  :args (s/cat :config ::spec/config))

(defn audit-graphql-interceptor
  "Interceptor that adds oulled user to the Lacinia context (requires user-graphql-interceptor first), required
   (but there is a leeway for the library maintainer to make it optional)."
  [config]
  (interceptors/audit-graphql-interceptor config))

(s/fdef audit-graphql-interceptor
  :args (s/cat :config ::spec/config))
