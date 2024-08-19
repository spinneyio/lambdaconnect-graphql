(ns lambdaconnect-graphql.interceptors 
  (:require [clojure.string :as str]
            [lambdaconnect-graphql.utils :as u]))

(defn db-grapqhl-interceptor
  [{:keys [db conn interceptor]}]
  (interceptor
   {:name ::database-graphql-interceptor
    :enter
    (fn [context]
      (assoc-in context [:request :lacinia-app-context :database] (db conn)))}))

(defn get-user-from-context
  [{:keys [pull]} context database]
  (let [user-uuid (some-> context :request :identity :user u/string->uuid)
        user (pull database '[*] [:app/uuid user-uuid])]
    user))

(defn user-graphql-interceptor
  [{:keys [interceptor] :as config}]
  (interceptor
   {:name ::user-graphql-interceptor
    :enter
    (fn [context]
      (let [database (-> context :request :lacinia-app-context :database)
            user (get-user-from-context config context database)]
        (assoc-in context [:request :lacinia-app-context :user] user)))}))

(defn audit-graphql-interceptor
  [{:keys [interceptor]}]
  (interceptor
   {:name ::audit-graphql-interceptor
    :enter
    (fn [{:keys [request] :as context}]
      (let [user (-> context
                     (get-in [:request :lacinia-app-context :user])
                     (select-keys [:db/id :user/username :user/email :user/additional-data :app/uuid]))
            audit-data
            {:audit/ip-address (if-let [ips (get-in request [:headers "x-forwarded-for"])]
                                 (-> ips (str/split #",") first)
                                 (:remote-addr request))
             :audit/user user
             :audit/http-method (some-> request :request-method name)
             :audit/http-path (some-> request :request-method name)
             :audit/http-agent (get-in request [:headers "user-agent"])}]
        (-> context
            (assoc-in [:request :lacinia-app-context :audit-data] audit-data)
            (assoc-in [:request :lacinia-app-context :user] user))))}))
