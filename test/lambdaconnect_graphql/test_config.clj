(ns lambdaconnect-graphql.test-config
  (:require [com.walmartlabs.lacinia :as lacinia]
            [com.walmartlabs.lacinia.constants :as constants]
            [com.walmartlabs.lacinia.resolve :as resolve]
            [com.walmartlabs.lacinia.schema :as lacinia-schema]
            [com.walmartlabs.lacinia.selection :as selection]
            [com.walmartlabs.lacinia.util :as util]
            [datomic.api :as d]
            [lambdaconnect-graphql.core :as core]
            [lambdaconnect-graphql.test-custom :as custom]
            [lambdaconnect-graphql.test-data :as data]
            [lambdaconnect-model.core :as lm]
            [lambdaconnect-model.scoping :as lm-scoping]
            [mount.core :as mount :refer [defstate]]
            [tiny-auth.db.session :as ta-db-session]
            [tiny-auth.db.user :as ta-db-user]))

(defstate entities-by-name
  :start (lm/entities-by-name "resources/model/current.xml"))

(defstate datomic-schema
  :start (->> entities-by-name
              lm/datomic-schema
              (concat custom/basic-schema)
              (mapv #(dissoc % :db/index))))

(defstate custom-schema
  :start (core/custom-schema custom/custom-input-objects-fields custom/custom-object-fields))

(defstate all-rules
  :start (-> "resources/model/scope.edn"
             slurp
             read-string
             lm-scoping/add-include-in-push-permission
             (custom/get-scoping-rules entities-by-name)
             (concat custom/custom-rules)))

(defstate user-scoping-permissions
  :start (custom/get-user-scoping-permissions "resources/model/scope.edn"))

(defstate graphql-model
  :start (core/graphql-model entities-by-name custom-schema custom/deprecated-fields))

(def db "datomic:mem://test")

(defstate conn
  :start (d/connect db))

(defstate config
  :start (core/lambdaconnect-graphql-config
          {:entities-by-name entities-by-name
           :datomic-schema datomic-schema
           :custom-schema custom-schema
           :conn conn
           :q d/q
           :pull d/pull
           :db d/db
           :with-db d/db
           :transact (fn [conn transaction] @(d/transact conn transaction))
           :with (fn [db transaction] (d/with db transaction))
           :interceptor nil
           :resolve-as resolve/resolve-as
           :parsed-query-key constants/parsed-query-key
           :selection-key constants/selection-key
           :selection-kind selection/selection-kind
           :answer-key-fns custom/answer-key-fns
           :custom-field-verifiers custom/custom-field-verifiers
           :custom-db-verifiers custom/custom-db-verifiers
           :custom-transaction-modifiers custom/custom-transaction-modifiers
           :custom-attributes custom/custom-attributes
           :custom-relationships custom/custom-relationships
           :user-rules-by-id (constantly all-rules)
           :get-scoping-permissions (constantly user-scoping-permissions)}))

(defstate resolvers
  :start (core/resolvers config))

(defstate schema
  :start (-> graphql-model
             (util/attach-resolvers resolvers)
             lacinia-schema/compile))

(defn execute-query
  "Cop out of endpoint, execute queries directly"
  [user-db-id query-string]
  (let [snapshot (d/db conn)
        user (d/pull snapshot '[*] user-db-id)]
    (lacinia/execute schema query-string nil
                     {:request {:lacinia-app-context
                                {:database snapshot
                                 :user user}}})))

(defn create-schema [conn]
  @(d/transact conn datomic-schema)
  @(d/transact conn ta-db-user/user-schema)
  @(d/transact conn ta-db-session/user-session-schema))

(defn create-test-database []
  (mount/start #'entities-by-name #'datomic-schema)
  (d/create-database db)
  (create-schema (d/connect db))
  (mount/stop))

(defn setup-test-environment [f]
  (create-test-database)
  (mount/start #'entities-by-name
               #'datomic-schema
               #'custom-schema
               #'all-rules
               #'user-scoping-permissions
               #'graphql-model
               #'conn
               #'config
               #'resolvers
               #'schema)
  (data/initialize-global-data config)
  (f)
  (d/delete-database db)
  (mount/stop))
