(ns lambdaconnect-graphql.relations
  (:require [clojure.set :as set]
            [lambdaconnect-graphql.constants :as constants]
            [lambdaconnect-graphql.utils :as u]))

(defn validate-obligatory-relations
  "If relation is obligatory and to-one, it must be connected to one entity.
   If relation is obligatory and to-many, it must be connected to at least one entity.

   For each object in input, check if each obligatory relation has an object on the other side."
  [{:keys [entities-by-name q]} flat-parsed-input user-scoped-ids db]
  (when-not constants/*ignore-obligatory-relations?*
    (let [other-id (fn [i] (symbol (str "?other-id-" i)))

          missing-relations
          (->> flat-parsed-input
               (group-by first)
               (map (fn [[entity objects]]
                      (let [entity-description (get entities-by-name entity)
                            uuids (map (comp :app/uuid second) objects)

                            relations
                            (->> entity-description
                                 :relationships
                                 (keep (fn [[attribute {:keys [inverse-entity inverse-name optional]}]]
                                         (when (not optional)
                                           (let [datomic-relation
                                                 (get-in entity-description [:datomic-relationships attribute])

                                                 inverse-datomic-relation
                                                 (get-in entities-by-name [inverse-entity :datomic-relationships inverse-name])]
                                             {:entity inverse-entity
                                              :other-id (other-id attribute)
                                              :namespaced-attribute (symbol entity attribute)
                                              :datomic-attr (or (u/->datomic-keyword datomic-relation)
                                                                (u/->datomic-keyword inverse-datomic-relation))
                                              :forward? datomic-relation})))))
                            relation->inverse-entity (->> relations (map (juxt (comp keyword :namespaced-attribute) :entity)) (into {}))

                            get-attribute-rule (fn [relation forward? other-id]
                                                 `(~'or-join [~'?id ~other-id]
                                                             (~'and ~(if forward?
                                                                       ['?id relation other-id]
                                                                       [other-id relation '?id])
                                                                    [~other-id :app/active true])
                                                             [(~'ground ~constants/empty-value) ~other-id]))

                            unscoped-ids (q `[:find ~'?uuid ~@(map #(list 'set (:other-id %)) relations)
                                              :keys ~'uuid ~@(map :namespaced-attribute relations)
                                              :in ~'$ ~'[?uuid ...]
                                              :where
                                              ;; or-join as in mutation/add-detached-objects should not be required,
                                              ;; since all objects exist in database
                                              ~'[?id :app/uuid ?uuid]
                                              ;; for inactive objects, we don't check obligatory relations,
                                              ;; otherwise removing objects would be very troublesome
                                              ~'[?id :app/active true]
                                              ~@(map (comp (partial apply get-attribute-rule)
                                                           (juxt :datomic-attr :forward? :other-id))
                                                     relations)]
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
                                                  (map (fn [[attr ids]] [attr (keep scoped-id->uuid ids)]) $) ;; keep discards ids out of scope!
                                                  (into {} $)
                                                  (assoc $ :uuid (:uuid response-row))))
                                              unscoped-ids)

                            entities-with-missing-attributes
                            (->> scoped-uuids
                                 (keep (fn [{:keys [uuid] :as result-map}]
                                         (let [missing-attributes
                                               (->> (dissoc result-map :uuid)
                                                    (keep (fn [[attr other-uuids]]
                                                            (when (empty? other-uuids)
                                                              attr))))]
                                           (when (seq missing-attributes)
                                             [uuid missing-attributes]))))
                                 (into {}))]
                        (when (seq entities-with-missing-attributes)
                          [entity entities-with-missing-attributes]))))
               (into {}))]
      (when (seq missing-relations)
        (u/fail "missingObligatoryRelations" missing-relations)))))
