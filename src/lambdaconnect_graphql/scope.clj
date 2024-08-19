(ns lambdaconnect-graphql.scope 
  (:require [clojure.set :as set]
            [failjure.core :as f]
            [lambdaconnect-graphql.constants :as constants]
            [lambdaconnect-graphql.utils :as u]))

(defn scoped-ids
  ([{:keys [user-rules-by-id] :as config} context user-id db] (scoped-ids config context user-id db (user-rules-by-id context user-id db)))
  ([{:keys [entities-by-name q]} _context user-id db rules]
   (->> entities-by-name
        (map (fn [[entity]] [entity (delay (->> (q `[:find ~'?db-id
                                                     :in ~'$ ~'% ~'?user-db-id
                                                     :where (~(symbol entity) ~'?user-db-id ~'?db-id)]
                                                   db rules user-id)
                                                (map first)
                                                set))]))
        (into {}))))

(defn validate-scoping-permissions
  [{:keys [entities-by-name q]} flat-parsed-input db scoping-permissions user-scoped-ids new?]
  (f/attempt-all
   [_existing-objects-permissions
    (u/fail
     "mutationEditingInaccessibleObjects"
     (some->> flat-parsed-input
              (remove (comp new? second))
              (group-by first)
              (map (fn [[entity objects]]
                     (let [uuids (map (comp :app/uuid second) objects)
                           scoped-ids @(get user-scoped-ids entity)
                           scoped-uuids (set (map first (q '[:find ?uuid :in $ [?id ...] :where [?id :app/uuid ?uuid]] db scoped-ids)))
                           inaccessible-uuids (remove scoped-uuids uuids)]
                       (when (seq inaccessible-uuids)
                         [entity inaccessible-uuids]))))
              (keep identity)
              seq
              (into {})))
    _objects-creation
    (u/fail
     "mutationCreatingUncreatableObjects"
     (some->> flat-parsed-input
              (filter (comp new? second))
              (group-by first)
              (keep (fn [[entity objects]]
                      (when-not (:create (get scoping-permissions entity))
                        [entity (map (comp :app/uuid second) objects)])))
              seq
              (into {})))
    _edit-fields-permissions
    (u/fail
     "mutationEditingWithoutPermissions"
     (some->> flat-parsed-input
              (remove (comp new? second))
              (group-by first)
              (keep (fn [[entity-name objects]]
                      (let [entity-description (get entities-by-name entity-name)
                            objects (map second objects)

                            ;; pull all attributes + uuids of all related objects
                            pull-pattern
                            (into '[*]
                                  (map (fn [[relation-name {:keys [inverse-entity inverse-name]}]]
                                         (let [attr (keyword entity-name relation-name)
                                               backward-attr (keyword inverse-entity inverse-name)]
                                           (if (get-in entity-description [:datomic-relationships relation-name])
                                             {attr [:app/uuid]}
                                             {[backward-attr :as attr] [:app/uuid]})))
                                       (:relationships entity-description)))
                            objects-in-db
                            (->> (q '[:find (pull ?id pattern)
                                      :in $ pattern [?uuid ...]
                                      :where [?id :app/uuid ?uuid]]
                                    db pull-pattern (map :app/uuid objects))
                                 (map (fn [[object]]
                                        [(:app/uuid object) object]))
                                 (into {}))

                            {:keys [modify protected-fields writable-fields]} (get scoping-permissions entity-name)
                            protected-fields (set protected-fields)
                            writable-fields (set writable-fields)

                            attr-is-changed?
                            (fn [object-uuid [attr value]]
                              (when (protected-fields (name attr))
                                (if (get-in entity-description [:attributes (name attr)])
                                  (when (not= value (get-in objects-in-db [object-uuid attr]))
                                    (name attr))
                                  ;; In case of uneditable relations, it is allowed to mention objects that
                                  ;; already are in this relation, but it's not possible to add new objects
                                  ;; or remove objects regardless of whether they actually are in relation
                                  (let [uuids-in-db (get-in objects-in-db [object-uuid attr])
                                        uuids-in-db (set (if (vector? uuids-in-db) uuids-in-db [uuids-in-db]))]
                                    (when (or (seq (:remove value))
                                              (seq (set/difference (set (:add value)) uuids-in-db)))
                                      (name attr))))))

                            invalid-objects
                            (keep (fn [object]
                                    (let [uuid (:app/uuid object)]
                                      (cond (not modify)
                                            ["unmodifiable" (:app/uuid object)]

                                            (seq protected-fields)
                                            (let [edited-protected-fields (filter #(attr-is-changed? uuid %) object)]
                                              (when (seq edited-protected-fields)
                                                ["editingProtectedFields" {"uuid" (:app/uuid object)
                                                                           "protectedFields" edited-protected-fields}]))

                                            (seq writable-fields)
                                            (let [edited-unwritable-fields (->> object
                                                                                (remove #(= (namespace (first %)) "app"))
                                                                                (filter #(attr-is-changed? uuid %)))]
                                              (when (seq edited-unwritable-fields)
                                                ["editingUnwritableFields" {"uuid" (:app/uuid object)
                                                                            "protectedFields" edited-unwritable-fields}])))))
                                  objects)]
                        (when (seq invalid-objects)
                          [entity-name invalid-objects]))))
              seq
              (into {})))]
   nil))

(defn validate-scoping-before-transaction
  "Check if user:
   * has access to edited objects,
   * does not create objects that it is not allowed to create,
   * for already existing objects modifications follow permissions values:
     - modify => if set to false, the object can't be edited at all,
     - protected-fields => if present, all fields except for these can be modified,
     - writable-fields => if present, only those fields (and app/*) can be modified"
  [{:keys [get-scoping-permissions] :as config} context flat-parsed-input user-db-id db user-scoped-ids new?]
  (when-not constants/*ignore-mutation-scoping?*
    (validate-scoping-permissions
     config
     flat-parsed-input
     db
     (get-scoping-permissions context user-db-id db)
     user-scoped-ids
     new?)))

(defn validate-scoping-after-transaction
  "Check if user has access to newly created objects
   and does not create inaccesible objects by removing a relation."
  [{:keys [q]} flat-parsed-input user-scoped-ids db]
  (when-not constants/*ignore-mutation-scoping?*
    (u/fail
     "mutationCreatingInaccessibleObjects"
     (some->> flat-parsed-input
              (group-by first)
              (map (fn [[entity objects]]
                     (let [uuids (map (comp :app/uuid second) objects)
                           scoped-ids @(user-scoped-ids entity)
                           scoped-uuids (set (map first (q '[:find ?uuid :in $ [?id ...] :where [?id :app/uuid ?uuid]] db scoped-ids)))
                           inaccessible-uuids (remove scoped-uuids uuids)]
                       (when (seq inaccessible-uuids)
                         [entity inaccessible-uuids]))))
              (keep identity)
              seq
              (into {})))))
