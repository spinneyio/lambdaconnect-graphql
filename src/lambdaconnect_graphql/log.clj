(ns lambdaconnect-graphql.log 
  (:require [lambdaconnect-graphql.utils :as u]))

(defn audit-datoms
  [config context timestamp]
  (let [audit-data (-> context
                       (get-in [:request :lacinia-app-context :audit-data])
                       (update :audit/user #(when-let [id (:db/id %)] {:db/id id})))]
    [(u/new-entity config nil timestamp
                   (merge {:db/id "datomic.tx"}
                          (->> audit-data (filter second) (into {}))))]))
