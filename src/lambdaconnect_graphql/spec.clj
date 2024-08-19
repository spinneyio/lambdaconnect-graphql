(ns lambdaconnect-graphql.spec
  (:require [clojure.spec.alpha :as s]
            [lambdaconnect-graphql.types :as types]))

(s/def :datomic/type (set (keys types/types-map)))
(s/def :datomic/cardinality #{:db.cardinality/one :db.cardinality/many})

(s/def :graphql/type (s/or :basic (set (keys types/inverse-types-map))
                           :list (s/and list?
                                        #(= 'list (first %))
                                        #(s/valid? :graphql/type (second %)))
                           :non-null (s/and list?
                                            #(= 'non-null (first %))
                                            #(s/valid? :graphql/type (second %)))))

(s/def ::description string?)

(s/def ::graphql-field (s/keys :req-un [:graphql/type ::description]))

(s/def :graphql/fields (s/map-of simple-keyword? ::graphql-field))
(s/def ::graphql-entity-description (s/nilable (s/keys :opt-un [::description :graphql/fields])))

(s/def ::custom-input-object-fields
  (s/nilable (s/map-of (s/and simple-keyword? #(re-matches #".*_input" (name %)))
                       ::graphql-entity-description)))

(s/def ::custom-object-fields (s/nilable (s/map-of simple-keyword? ::graphql-entity-description)))

(s/def ::deprecated-fields (s/nilable (s/map-of qualified-keyword? string?)))


;; Spec of various helper structures, often used as arguments in supplied functions

(s/def ::types-info-map (s/map-of uuid? string?))

(s/def ::flat-parsed-input (s/coll-of (s/cat :entity string? :object map?)))

;; map {entity -> delay (set of db ids)}
(s/def ::user-scoped-ids (s/map-of string? (s/and delay? #(set? @%) (s/coll-of int?))))

;; Config spec

(s/def ::answer-key-fns (s/nilable (s/map-of qualified-keyword? (s/fspec :args (s/cat :arg any?)
                                                                   :fn #(or (nil? (:ret %)) (= (-> % :args :arg type)
                                                                                               (-> % :ret type)))))))

(s/def ::custom-field-verifiers (s/nilable (s/map-of qualified-keyword? ifn?)))

(s/def :custom-attribute/fn ifn?)
(s/def ::custom-attributes (s/nilable (s/map-of qualified-keyword? (s/keys :req-un [:datomic/type
                                                                                    :datomic/cardinality
                                                                                    :custom-attribute/fn]))))

(s/def :custom-relationship/to-many? boolean?)
(s/def :custom-relationship/inverse-entity string?)
(s/def :custom-relationship/inverse-relationship qualified-keyword?)
(s/def :custom-relationship/fn ifn?)
(s/def ::custom-relationships (s/nilable (s/map-of qualified-keyword? (s/keys :req-un [:custom-relationship/to-many?
                                                                                       :custom-relationship/inverse-entity
                                                                                       :custom-relationship/inverse-relationship
                                                                                       :custom-relationship/fn]))))

(s/def ::force-sequential-pull-execution? boolean?)

(s/def ::config (s/keys :req-un [;; ---- Refer to another library or function for details
                                 ::entities-by-name ;; lambdaconnect-model
                                 ::custom-schema ;; core/custom-schema
                                 ;; Inspect implementation of com.walmartlabs.lacinia.executor/build-selections-map
                                 ;; and see what function is applied in case (X selection),
                                 ;; put X here (as an example, in 0.16.1 version of lacinia it is keyword :selection-type
                                 ;; and in 1.2.2 it is com.walmartlabs.lacinia.selection/selection-kind)
                                 ::selection-kind

                                 ;; ---- Datomic dependencies: connection and functions
                                 ::conn
                                 ::q ;; d/q
                                 ::pull ;; d/pull
                                 ::db ;; d/db
                                 ;; with-db is a function is which takes a conn and returns a database suitable to be passed
                                 ;; to the d/with function.
                                 ;; In Datomic Cloud, this is d/with-db and in Datomic Pro, this is d/db.
                                 ::with-db
                                 ;; To make use of both Datomic Cloud and Datomic Pro, wrap d/transact and d/with in a function.
                                 ;; Final function is expected to take 2 arguments: Datomic connection (transact)/with-db-result (with) and transaction body
                                 ;; and return transaction info.
                                 ;; In case of Datomic Pro, this would most likely be
                                 ;; transact -> (fn [conn transaction] @(d/transact conn transaction))
                                 ;; with -> (fn [db transaction] (d/with db transaction))
                                 ;; and in case of Datomic Cloud, this would most likely be
                                 ;; transact -> (fn [conn transaction] (d/transact conn {:tx-data transaction}))
                                 ;; with -> (fn [db transaction] (d/with db {:tx-data transaction}))
                                 ::transact
                                 ::with

                                 ;; ---- Pedestal function
                                 ::interceptor ;; io.pedestal.interceptor/interceptor

                                 ;; ---- Lacinia functions and keywords
                                 ::resolve-as ;; com.walmartlabs.lacinia.resolve/resolve-as
                                 ::parsed-query-key ;; com.walmartlabs.lacinia.constants/parsed-query-key
                                 ::selection-key ;; com.walmartlabs.lacinia.constants/selection-key

                                 ;; ---- Model dependent:
                                 ;; answer-key-fns is a simple and primitive way to modify returned field.
                                 ;; It should be a map. Each key is a namepsaced attribute and value is a function
                                 ;; which takes only one argument: value of the attribute (possibly nil).
                                 ;; The function should return value of the same type (or nil, if model permits).
                                 ;; Example usage: There is a private S3 bucket in which we store photos.
                                 ;; In database, we only store photo keys and the function turns them into presigned URLs.
                                 ::answer-key-fns

                                 ;; custom-field-verifiers is a simple and primitive way to validate input values where xml is not sufficient.
                                 ;; It should be a map. Each key is a namepsaced attribute and value is a function
                                 ;; which takes only one argument: map with keys :value, :entities-by-name and :object.
                                 ;; It should return nil/false if value is correct or a truthy object
                                 ;; which explains the error (it will be sent in response) - it doesn't have to be a failjure error.
                                 ;; Example usage: Price of a product can be either 0 or a number greater than 100.
                                 ::custom-field-verifiers

                                 ;; custom-db-verifiers is an advanced way of verifying database invariants after applying transaction
                                 ;; It should be a map. Each key is a string label and value is a function
                                 ;; which takes only one argument: map with keys :entities-by-name, :flat-parsed-input, :initial-db, :db, :types-info-map, :mock?, :user-db-id, :context
                                 ;; Initial-db is a snapshot BEFORE applying changes and db is a snapshot AFTER applying changes.
                                 ;; :types-info-map is a map with uuids as keys and types (entity string name) as values (spec ::types-info-map).
                                 ;; :mock? is true if mutation is a mock upsert and false otherwise
                                 ;; It should return nil/false if value is correct or a truthy object
                                 ;; which explains the error (it will be sent in response) - it doesn't have to be a failjure error.
                                 ::custom-db-verifiers

                                 ;; custom-transaction-modifiers is an advanced way of applying additional changes to database
                                 ;; It should be a list of pairs (unlike others, order is important here - modifiers are processed top to bottom). Each key is a string label and value is a function
                                 ;; which takes only one argument: map with keys :transaction, :entities-by-name, :flat-parsed-input, :db, :initial-db, :now, :types-info-map, :context.
                                 ;; Use :now as timestamp (for fields updatedAt/createdAt), to avoid date conflicts.
                                 ;; Each function should return either a (possibly modified) transaction or a failjure error to abort all changes.
                                 ;; Return value of the modifier should be a map with at least a key :transaction, but might contain also other fields, such as :types-info-map
                                 ;; (the modifier is advised to update this map if new entities were created!) or also other keys that will be passed to another modifiers (result is merged with default map arg).
                                 ;; Intial-db is a snapshot before applying any changes and db is a snapshot after applying initial changes (if first modifier) or the result of the previous modifier.
                                 ::custom-transaction-modifiers

                                 ;; custom-attributes describes additional fields which are not in entites-by-name, but are in custom-schema
                                 ;; It should be a map, where keys are namespaced attributes (entity/attr) and values are maps with three keys:
                                 ;; :type (datomic type), :cardinality (datomic cardinality) and :fn - a function which takes as input one map with keys :db, :ids, :rules, :scoped-ids
                                 ;; :rules is the result of calling (user-rules-by-id user-db-id db) and :scoped-ids is speced by ::user-scoped-ids.
                                 ;; ids is a collection of db ids for which the attribute should be computed.
                                 ;; The function should return a map {id (from ids) -> attribute value (possibly :lambdaconnect-graphql.constants/empty-value)}
                                 ::custom-attributes

                                 ;; custom-relationships describe additional relationships which are not in entites-by-name, but are in custom-schema
                                 ;; It should be a map, where keys are namespaced relaitons (entity/rel) and values are maps with four keys:
                                 ;; :to-many (true/false), :inverse-entity (string), :inverse-relationship (namespaced keyword, entity/rel), :fn - a function which takes the same args as custom-attributes fn
                                 ;; and is expected to return a map {id -> id} or {id -> set of ids} (depending on to-many)
                                 ::custom-relationships

                                 ;; user-rules-by-id is a function which takes two args: user-id and snapshot and returns set of rules to be used in Datomic scoping queries
                                 ::user-rules-by-id

                                 ;; get-scoping-permissions is a function which takes three args: context, user-db-id, db and returns scoping permissions (modify/create/writable-fields/protected-fields) as a nested-map
                                 ::get-scoping-permissions
                                 
                                 ;; ---- ,,Optional'' arguments
                                 ;; log-mutation is a function called after upsert fails or finishes successfully (may be nil)
                                 ;; Arguments: context (from Lacinia), args (of query), mock? (true/false), error (exception or nil if successful), timestamp, other-log-data (collected during create-upsert, check implementation for details)
                                 ;; You can pass nil when constructiong config, it will be replaced with (constantly nil)
                                 ::log-mutation

                                 ;; boolean flag, multiple entities are pulled sequentially if set to true
                                 ::force-sequential-pull-execution?

                                 ;; ---- Computed from another parts of the config and kept for performance and convenience.
                                 ::custom-resolvers
                                 ::default-values
                                 ::datomic-attrs
                                 ::datomic-attrs-only
                                 ::datomic-relationships-only]))

