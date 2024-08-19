(ns lambdaconnect-graphql.resolvers
  (:require [lambdaconnect-graphql.model :as model]
            [lambdaconnect-graphql.mutation :as mutation]
            [lambdaconnect-graphql.patterns :as patterns]
            [lambdaconnect-graphql.pull :as pull]))

(defn wrap-resolved-pull
  [{:keys [selection-key] :as config} resolver-fn]
  (fn [context args value]
    (let [field (:field-name (selection-key context))
          val (field value)]
      ;; Two non-standard cases:
      ;; * field is not in the value map and needs separate resolver - rejected by contains?,
      ;; * field is in the map and value is nil - will be accepted by top-keys-covered?, field was optional
      (if (and (contains? value field)
               (patterns/top-keys-covered? context config val))
        val
        (resolver-fn context args val)))))

(defn relation-resolvers
  [{:keys [entities-by-name] :as config}]
  (->> entities-by-name
       (mapcat (fn [[entity-name {:keys [relationships datomic-relationships]}]]
                 (map (fn [[relationship-name {:keys [to-many inverse-name inverse-entity]}]]
                        (let [forward-k (keyword entity-name relationship-name)
                              forward? (contains? datomic-relationships relationship-name)
                              backward-k (if forward?
                                           forward-k
                                           (keyword inverse-entity (str "_" inverse-name)))
                              db-fn (if forward?
                                      (if to-many pull/pull-refs pull/pull-ref)
                                      (if to-many pull/reverse-pull-refs pull/reverse-pull-ref))]
                          [forward-k
                           (wrap-resolved-pull
                            config
                            (fn [context _args value]
                              (db-fn config
                                     (get-in context [:request :lacinia-app-context :database])
                                     (Long. (:id value))
                                     (patterns/pull-pattern context config)
                                     backward-k
                                     entities-by-name)))]))
                      relationships)))
       (into {})))

(defn queries-by-uuid
  [{:keys [entities-by-name] :as config}]
  (->> entities-by-name
       (map (fn [[entity-name]]
              [(keyword "query" (str entity-name "-by-uuid"))
               (wrap-resolved-pull
                config
                (fn [context args _value]
                  (let [user-db-id (get-in context [:request :lacinia-app-context :user :db/id])
                        seq-pull-structure (patterns/sequential-pull-structure context config)]
                    (pull/pull-by-uuids config
                                        context
                                        (get-in context [:request :lacinia-app-context :database])
                                        (:uuid args)
                                        user-db-id
                                        seq-pull-structure))))]))
       (into {})))

(defn queries-all
  [{:keys [entities-by-name] :as config}]
  (->> entities-by-name
       (map (fn [[entity-name]]
              [(keyword "query" (str entity-name "-all"))
               (wrap-resolved-pull
                config
                (fn [context args _value]
                  (let [user-db-id (get-in context [:request :lacinia-app-context :user :db/id])
                        database (get-in context [:request :lacinia-app-context :database])
                        seq-pull-structure (patterns/sequential-pull-structure context config)
                        pull-all-result (pull/pull-all database
                                                       config
                                                       context
                                                       entity-name
                                                       user-db-id
                                                       seq-pull-structure
                                                       :query-sorts (:sort args)
                                                       :query-pagination (:pagination args)
                                                       :query-filter (:filter args))]
                    pull-all-result)))]))
       (into {})))

(defn simple-upserts
  [{:keys [entities-by-name custom-schema] :as config}]
  (let [entities-by-name-with-custom-input (model/entities-by-name-with-custom-input entities-by-name custom-schema)
        config (assoc config :entities-by-name entities-by-name-with-custom-input)]
    (->> entities-by-name
         (mapcat (fn [[entity-name entity]]
                   [[(keyword "mutation" (str "upsert-" entity-name))
                     (wrap-resolved-pull config (mutation/create-upsert config entity false))]
                    [(keyword "mutation" (str "mock-upsert-" entity-name))
                     (wrap-resolved-pull config (mutation/create-upsert config entity true))]]))
         (into {}))))

(defn resolvers
  [config]
  (merge (relation-resolvers config)
         (queries-all config)
         (queries-by-uuid config)
         (simple-upserts config)))
