(ns lambdaconnect-graphql.test-mutation
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.walk :refer [postwalk]]
            [failjure.core :as f]
            [lambdaconnect-graphql.core :refer [empty-value]]
            [lambdaconnect-graphql.mutation :as mutation]
            [lambdaconnect-graphql.patterns :as patterns]
            [lambdaconnect-graphql.pull :as pull]
            [lambdaconnect-graphql.test-config :as config]
            [lambdaconnect-graphql.utils :as u]
            [lambdaconnect-model.core :as lm]
            [lambdaconnect-graphql.test-graphql-utils :as tgu]))

(use-fixtures :each config/setup-test-environment)

(defn count-elements [structure]
  (if (map? structure)
    (reduce + (map count-elements (vals structure)))
    (if (vector? structure)
      (reduce + (map count-elements structure))
      1)))

(defn map-vectors-to-sets [structure]
  (if (map? structure)
    (into {} (for [[k v] structure] [k (map-vectors-to-sets v)]))
    (if (vector? structure)
      (into #{} (map map-vectors-to-sets structure))
      structure)))

(defn deep-compare [structure1 structure2]
  (cond (and (map? structure1) (map? structure2))
        (and (is (= (count structure1) (count structure2)) (str "map " structure1 structure2))
             (every? (fn [[k1 v1]]
                       (let [[k2 v2] (find structure2 k1)]
                         (and k2 (deep-compare v1 v2))))
                     structure1))
        (and (set? structure1) (set? structure2))
        (and (is (= (count structure1) (count structure2)) (str "set " structure1 structure2))
             (every? (fn [x] (contains? structure2 x)) structure1))
        :else (is (= structure1 structure2))))

(defn add-remove-map
  [add remove]
  {:add (map #(hash-map :app/uuid %) add)
   :remove (map #(hash-map :app/uuid %) remove)})

(deftest validate-value
  (testing "String length"
    (is (every? nil? (mutation/validate-value config/config "RAPrice" nil "currency" "YES")))
    (is (some some? (mutation/validate-value config/config "RAPrice" nil "currency" "NO")))
    (is (some some? (mutation/validate-value config/config "RAPrice" nil "currency" "NOPE"))))
  (testing "Regex"
    (is (every? nil? (mutation/validate-value config/config "RARestaurant" nil "unitSystem" "metric")))
    (is (some some? (mutation/validate-value config/config "RARestaurant" nil "unitSystem" "quack"))))
  (testing "Min and max"
    (is (every? nil? (mutation/validate-value config/config "RAVatPercentage" nil "percentage" 23)))
    (is (some some? (mutation/validate-value config/config "RAVatPercentage" nil "percentage" -23)))
    (is (some some? (mutation/validate-value config/config "RAVatPercentage" nil "percentage" 230)))))

(deftest add-and-remove
  (is (f/ok? (mutation/add-and-remove (add-remove-map nil nil))))
  (is (f/ok? (mutation/add-and-remove (add-remove-map [1] [2]))))
  (is (f/failed? (mutation/add-and-remove (add-remove-map [1 3] [2 3])))))

(deftest merge-values
  (is (f/ok? (mutation/merge-values nil nil)))
  (is (f/ok? (mutation/merge-values 5 5)))
  (is (f/ok? (mutation/merge-values (add-remove-map [1 2] [4 5]) (add-remove-map [1 3] [4 6]))))
  (is (f/failed? (mutation/merge-values 5 nil)))
  (is (f/failed? (mutation/merge-values 5 6)))
  (is (f/failed? (mutation/merge-values (add-remove-map [1 2] [4 5]) (add-remove-map [1 3] [2 6])))))

(deftest collect-input-types
  (is (f/ok? (mutation/collect-input-types
              config/config
              [["RAOwner" {:app/uuid #uuid "a2dbc169-a7c7-49b7-9534-4acec9ceeaf5"
                           :RAOwner/email "first-user@gmail.com"
                           :RAOwner/internalUserId #uuid "c6f7fa5a-880b-48e8-9b75-9ab7a3014307"
                           :RAOwner/name "Jan"
                           :RAOwner/surname "Kowalski"}]
               ["RAAllergen" {:app/uuid #uuid "a2dbc169-a7c7-49b7-9534-4acec9ceeaf4"
                              :RAAllergen/name "allergen"}]])))
  (is (f/failed? (mutation/collect-input-types
                  config/config
                  [["RAOwner" {:app/uuid #uuid "a2dbc169-a7c7-49b7-9534-4acec9ceeaf5"
                               :RAOwner/email "first-user@gmail.com"
                               :RAOwner/internalUserId #uuid "c6f7fa5a-880b-48e8-9b75-9ab7a3014307"
                               :RAOwner/name "Jan"
                               :RAOwner/surname "Kowalski"}]
                   ["RAAllergen" {:app/uuid #uuid "a2dbc169-a7c7-49b7-9534-4acec9ceeaf5" ;; same uuid
                                  :RAAllergen/name "allergen"}]]))))

(deftest compare-types
  (is (f/ok? (mutation/compare-types
              {#uuid "a2dbc169-a7c7-49b7-9534-4acec9ceeaf5" "RAOwner"
               #uuid "a2dbc169-a7c7-49b7-9534-4acec9ceeaf4" "RAAllergen"}
              {#uuid "a2dbc169-a7c7-49b7-9534-4acec9ceeaf5" "RAOwner"})))
  (is (f/failed? (mutation/compare-types
                  {#uuid "a2dbc169-a7c7-49b7-9534-4acec9ceeaf5" "RAOwner"}
                  {#uuid "a2dbc169-a7c7-49b7-9534-4acec9ceeaf5" "RAAllergen"}))))

(deftest merge-input
  (is (= (mutation/merge-input
          [["RAOwner" {:app/uuid #uuid "a2dbc169-a7c7-49b7-9534-4acec9ceeaf5"
                       :RAOwner/email "first-user@gmail.com"
                       :RAOwner/internalUserId #uuid "c6f7fa5a-880b-48e8-9b75-9ab7a3014307"
                       :RAOwner/name "Jan"}]
           ["RAOwner" {:app/uuid #uuid "a2dbc169-a7c7-49b7-9534-4acec9ceeaf5"
                       :RAOwner/email "first-user@gmail.com"
                       :RAOwner/internalUserId #uuid "c6f7fa5a-880b-48e8-9b75-9ab7a3014307"
                       :RAOwner/surname "Kowalski"}]])
         [["RAOwner" {:app/uuid #uuid "a2dbc169-a7c7-49b7-9534-4acec9ceeaf5"
                      :RAOwner/email "first-user@gmail.com"
                      :RAOwner/internalUserId #uuid "c6f7fa5a-880b-48e8-9b75-9ab7a3014307"
                      :RAOwner/name "Jan"
                      :RAOwner/surname "Kowalski"}]]))
  (is (f/failed? (mutation/merge-input [["RAOwner" {:app/uuid #uuid "a2dbc169-a7c7-49b7-9534-4acec9ceeaf5"
                                                    :RAOwner/email "first-user@gmail.com"
                                                    :RAOwner/internalUserId #uuid "c6f7fa5a-880b-48e8-9b75-9ab7a3014307"
                                                    :RAOwner/name "Jan"
                                                    :RAOwner/surname "Kowalski"}]
                                        ["RAOwner" {:app/uuid #uuid "a2dbc169-a7c7-49b7-9534-4acec9ceeaf5"
                                                    :RAOwner/email "first-user@gmail.com"
                                                    :RAOwner/internalUserId #uuid "c6f7fa5a-880b-48e8-9b75-9ab7a3014307"
                                                    :RAOwner/name "Janusz"
                                                    :RAOwner/surname "Kowalski"}]]))))

(deftest parsing-graphql-input-to-push-input
  (let [ld-entities-by-name (lm/entities-by-name "resources/model/loud-delivery.xml")
        entity (get ld-entities-by-name "LDCMSUser")]
    (testing "Entity without relationships:"
      (let [graphql-input {:entity "LDCMSUser"
                           :data {:fullName "dzban"
                                  :role "DriverManager"
                                  :internalUserId "a3b93c25-e601-4999-8da7-acab6bfca80e"
                                  :uuid "e3b93c25-e601-4999-8da7-acab6bfca80e"
                                  :active true
                                  :enabled true
                                  :email "dzban@gmail.com"
                                  :createdAt "2020-07-31T08:04:45.912-00:00"
                                  :updatedAt "2020-07-31T08:04:45.912-00:00"}}
            push-input {"LDCMSUser" [{"fullName" "dzban"
                                      "role" "DriverManager"
                                      "internalUserId" "a3b93c25-e601-4999-8da7-acab6bfca80e"
                                      "uuid" "e3b93c25-e601-4999-8da7-acab6bfca80e"
                                      "active" true
                                      "enabled" true
                                      "email" "dzban@gmail.com"
                                      "createdAt" "2020-07-31T08:04:45.912-00:00"
                                      "updatedAt" "2020-07-31T08:04:45.912-00:00"}]}
            parse-input-result (mutation/parse-input
                                (:data graphql-input)
                                entity
                                {:entities-by-name ld-entities-by-name})]
        (is (= push-input parse-input-result))))
    (testing "Entity with one relationship (:to-one):"
      (let [graphql-input {:entity "LDCMSUser"
                           :data {:fullName "dzban"
                                  :role "DriverManager"
                                  :internalUserId "a3b93c25-e601-4999-8da7-acab6bfca80e"
                                  :uuid "e3b93c25-e601-4999-8da7-acab6bfca80e"
                                  :active true
                                  :enabled true
                                  :email "dzban@gmail.com"
                                  :createdAt "2020-07-31T08:04:45.912-00:00"
                                  :updatedAt "2020-07-31T08:04:45.912-00:00"
                                  :restaurant {:active true
                                               :createdAt "2020-07-31T08:04:45.912-00:00"
                                               :updatedAt "2020-07-31T08:04:45.912-00:00"
                                               :uuid "5c96a28f-c0d1-46a2-8c12-579c67c9b896"}}}
            push-input {"LDCMSUser" [{"fullName" "dzban"
                                      "role" "DriverManager"
                                      "internalUserId" "a3b93c25-e601-4999-8da7-acab6bfca80e"
                                      "uuid" "e3b93c25-e601-4999-8da7-acab6bfca80e"
                                      "active" true
                                      "enabled" true
                                      "email" "dzban@gmail.com"
                                      "createdAt" "2020-07-31T08:04:45.912-00:00"
                                      "updatedAt" "2020-07-31T08:04:45.912-00:00"
                                      "restaurant" "5c96a28f-c0d1-46a2-8c12-579c67c9b896"}]
                        "LDRestaurant" [{"active" true
                                         "createdAt" "2020-07-31T08:04:45.912-00:00"
                                         "updatedAt" "2020-07-31T08:04:45.912-00:00"
                                         "uuid" "5c96a28f-c0d1-46a2-8c12-579c67c9b896"}]}
            parse-input-result (mutation/parse-input
                                (:data graphql-input)
                                entity
                                {:entities-by-name ld-entities-by-name})]
        (is (= push-input parse-input-result))))

    (testing "Entity with relationship to the object with another relationship (:to-many):"
      (let [graphql-input {:entity "LDCMSUser"
                           :data {:fullName "dzban"
                                  :role "DriverManager"
                                  :internalUserId "a3b93c25-e601-4999-8da7-acab6bfca80e"
                                  :uuid "e3b93c25-e601-4999-8da7-acab6bfca80e"
                                  :active true
                                  :createdAt "2020-07-31T08:04:45.912-00:00"
                                  :updatedAt "2020-07-31T08:04:45.912-00:00"
                                  :enabled true
                                  :email  "dzban@gmail.com"
                                  :restaurant {:active true
                                               :createdAt "2020-07-31T08:04:45.912-00:00"
                                               :updatedAt "2020-07-31T08:04:45.912-00:00"
                                               :uuid "5c96a28f-c0d1-46a2-8c12-579c67c9b896"
                                               :areas {:add [{:active true
                                                              :createdAt "2020-07-31T08:04:45.912-00:00"
                                                              :updatedAt "2020-07-31T08:04:45.912-00:00"
                                                              :uuid "a157114b-272c-4ffc-a006-364e5a133da5"
                                                              :code "code1"}
                                                             {:active true
                                                              :createdAt "2020-07-31T08:04:45.912-00:00"
                                                              :updatedAt "2020-07-31T08:04:45.912-00:00"
                                                              :uuid "4a025fc1-010c-43dc-ba47-209334a2f727"
                                                              :code "code2"}]
                                                       :remove []}}}}
            push-input {"LDCMSUser" [{"fullName" "dzban"
                                      "role" "DriverManager"
                                      "internalUserId" "a3b93c25-e601-4999-8da7-acab6bfca80e"
                                      "uuid" "e3b93c25-e601-4999-8da7-acab6bfca80e"
                                      "active" true
                                      "enabled" true
                                      "email" "dzban@gmail.com"
                                      "createdAt" "2020-07-31T08:04:45.912-00:00"
                                      "updatedAt" "2020-07-31T08:04:45.912-00:00"
                                      "restaurant" "5c96a28f-c0d1-46a2-8c12-579c67c9b896"}]
                        "LDRestaurant" [{"active" true
                                         "createdAt" "2020-07-31T08:04:45.912-00:00"
                                         "updatedAt" "2020-07-31T08:04:45.912-00:00"
                                         "uuid" "5c96a28f-c0d1-46a2-8c12-579c67c9b896"
                                         "areas" {:add ["a157114b-272c-4ffc-a006-364e5a133da5" "4a025fc1-010c-43dc-ba47-209334a2f727"]
                                                  :remove []}}]
                        "LDArea" [{"active" true
                                   "createdAt" "2020-07-31T08:04:45.912-00:00"
                                   "updatedAt" "2020-07-31T08:04:45.912-00:00"
                                   "uuid" "a157114b-272c-4ffc-a006-364e5a133da5"
                                   "code" "code1"}
                                  {"active" true
                                   "createdAt" "2020-07-31T08:04:45.912-00:00"
                                   "updatedAt" "2020-07-31T08:04:45.912-00:00"
                                   "uuid" "4a025fc1-010c-43dc-ba47-209334a2f727"
                                   "code" "code2"}]}
            parse-input-result (mutation/parse-input
                                (:data graphql-input)
                                entity
                                {:entities-by-name ld-entities-by-name})]
        (is (= push-input parse-input-result))))))

(deftest new-entity
  (let [now (u/now)
        entity {:RAMenu/name "Menu"
                :db/id "menu"
                :RAMenu/booleanFlag false}
        new-entity-with-all-data (u/new-entity config/config "RAMenu" now entity true)
        new-entity-without-all-data (u/new-entity config/config "RAMenu" now entity)
        photo-entity {:upload/s3-upload-key "photo-key"
                      :upload/size 32
                      :db/id "new-photo"}
        new-photo-entity (u/new-entity config/config nil now photo-entity)]
    (is (= {:RAMenu/name "Menu"
            :RAMenu/archived false
            :RAMenu/isMainMenuOfARestaurant false
            :RAMenu/booleanFlag false
            :app/active true
            :app/createdAt now
            :app/updatedAt now
            :db/id "menu"
            :RAMenu/ident__ true}
           (dissoc new-entity-with-all-data :app/uuid)))
    (is (:app/uuid new-entity-with-all-data))
    (is (= {:RAMenu/archived false
            :RAMenu/isMainMenuOfARestaurant false
            :app/active true
            :app/createdAt now
            :app/updatedAt now
            :db/id "menu"
            :RAMenu/ident__ true}
           (dissoc new-entity-without-all-data :app/uuid)))
    (is (:app/uuid new-entity-without-all-data))
    (is (= {:upload/s3-upload-key "photo-key"
            :upload/size 32
            :app/active true
            :app/createdAt now
            :app/updatedAt now
            :db/id "new-photo"}
           (dissoc new-photo-entity :app/uuid)))
    (is (:app/uuid new-photo-entity))))

(deftest old-perform-sequential-pull-test
  (let [{:keys [conn q db transact]} config/config
        user [{:user/username "restaurant.owner@spinney.io",
               :user/email "restaurant.owner@spinney.io",
               :db/id "restaurant.owner@spinney.io",
               :user/additional-data "{}",
               :user/confirmed true,
               :app/uuid #uuid "06213c92-588f-4d65-81fa-d97030aeb0fc",
               :user/deactivated false}
              {:user/username "maks.sulima@spinney.io",
               :user/email "maks.sulima@spinney.io",
               :db/id "maks.sulima@spinney.io",
               :user/additional-data "{}",
               :user/confirmed true,
               :app/uuid #uuid "06213c92-2137-4d65-81fa-d97030aeb0fc",
               :user/deactivated false}]
        owner [{:RAOwner/ident__ true,
                :app/updatedAt #inst "2022-03-25T23:09:16.828-00:00",
                :app/createdAt #inst "2022-02-14T13:03:58.643-00:00",
                :RAOwner/name "Marylou",
                :RAOwner/email "restaurant.owner@spinney.io",
                :app/active true,
                :db/id "Marylou",
                :RAOwner/internalUserId #uuid "06213c92-588f-4d65-81fa-d97030aeb0fc",
                :RAOwner/surname "Baumer",
                :app/uuid #uuid "2d0555f8-90a3-4a5d-97e5-693164365dd4"}
               {:RAOwner/ident__ true,
                :app/updatedAt #inst "2022-03-25T23:09:16.828-00:00",
                :app/createdAt #inst "2022-02-14T13:03:58.643-00:00",
                :RAOwner/name "maks",
                :RAOwner/email "maks.sulima@spinney.io",
                :app/active true,
                :db/id "maks",
                :RAOwner/internalUserId #uuid "06213c92-2137-4d65-81fa-d97030aeb0fc",
                :RAOwner/surname "Baumer",
                :app/uuid #uuid "2d0555f8-90a3-4a5d-97e5-693164165dd4"}]
        employees [{:RAEmployee/email "adullardb@newsvine.com",
                    :app/updatedAt #inst "2022-08-03T16:59:27.169-00:00",
                    :RAEmployee/name "Angelina",
                    :app/createdAt #inst "2022-01-13T01:54:08.961-00:00",
                    :RAEmployee/ident__ true,
                    :RAEmployee/surname "Dullard",
                    :RAEmployee/internalUserId #uuid "8662bab4-7f96-4cef-a65b-6ec28fb5be6b",
                    :app/active true,
                    :db/id "Dullard",
                    :RAEmployee/owner "Marylou",
                    :app/uuid #uuid "7586a4e5-e3d2-4307-80e5-9544ba454db4"}
                   {:RAEmployee/email "frichind@mlb.com",
                    :app/updatedAt #inst "2022-08-13T15:48:55.740-00:00",
                    :RAEmployee/name "Frasco",
                    :app/createdAt #inst "2021-12-03T04:29:42.900-00:00",
                    :RAEmployee/ident__ true,
                    :RAEmployee/surname "Richin",
                    :RAEmployee/internalUserId #uuid "55be9917-4e7d-4a32-b60b-edde4b59d36c",
                    :app/active true,
                    :db/id "Richin",
                    :RAEmployee/owner "Marylou",
                    :app/uuid #uuid "71389067-2bdc-415d-b624-8f78f880ad98"}
                   {:RAEmployee/email "frichind@gmail.com",
                    :app/updatedAt #inst "2022-08-13T15:48:55.740-00:00",
                    :RAEmployee/name "Jan",
                    :app/createdAt #inst "2021-12-03T04:29:42.900-00:00",
                    :RAEmployee/ident__ true,
                    :RAEmployee/surname "Paweł",
                    :RAEmployee/internalUserId #uuid "11be9917-4e7d-4a32-b60b-edde4b59d36c",
                    :app/active true,
                    :db/id "Jan",
                    :RAEmployee/owner "Marylou",
                    :app/uuid #uuid "71389067-2137-415d-b624-8f78f880ad98"}
                   {:RAEmployee/email "kbochnak@gmail.com",
                    :app/updatedAt #inst "2022-08-13T15:48:55.740-00:00",
                    :RAEmployee/name "Krzysztof",
                    :app/createdAt #inst "2021-12-03T04:29:42.900-00:00",
                    :RAEmployee/ident__ true,
                    :RAEmployee/surname "Koźmiński",
                    :RAEmployee/internalUserId #uuid "123e9917-4e7d-4a32-b60b-edde4b59d36c",
                    :app/active true,
                    :db/id "Krzysztof",
                    :RAEmployee/owner "Marylou",
                    :app/uuid #uuid "71389067-2137-123d-b624-8f78f880ad98"}]
        restauransts [{:RARestaurant/name "Frasier"
                       :RARestaurant/owner "Marylou"
                       :RARestaurant/employees
                       ["Dullard" "Richin"],
                       :RARestaurant/subscriptionActive true
                       :db/id "Frasier",
                       :RARestaurant/ident__ true,
                       :app/active true,
                       :app/uuid #uuid "c5c54cee-6217-4e12-826d-8831e7c9e91d"}]
        ingredients [{:RAIngredient/name "pomidor47",
                      :RAIngredient/ident__ true,
                      :RAIngredient/restaurant "Frasier",
                      :db/id "pomidor47",
                      :app/active true,
                      :app/uuid #uuid "cc68b2ee-fc04-4198-94b6-bf2da23552f7"}
                     {:RAIngredient/name "pomidor48"
                      :RAIngredient/ident__ true,
                      :RAIngredient/restaurant "Frasier",
                      :db/id "pomidor48",
                      :app/active true,
                      :app/uuid #uuid "923179b9-af67-4636-988d-e0c2aa306de6"}
                     {:RAIngredient/name "pomidor49"
                      :RAIngredient/ident__ true,
                      :RAIngredient/restaurant "Frasier"
                      :db/id "pomidor49",
                      :app/active true,
                      :app/uuid #uuid "b4ea0df6-8451-4ee8-b56d-60f005001e0f"}]
        data (concat user owner employees restauransts ingredients)
        _ (transact conn data)
        db-snapshot (db conn)
        user-db-id (ffirst (q '[:find ?db-id :where [?db-id :user/email "restaurant.owner@spinney.io"]] db-snapshot))
        all-rules config/all-rules]

    (testing "count selection tree test"
      (let [actual-result (pull/perform-sequential-pull
                           config/config
                           nil
                           db-snapshot
                           (patterns/sequential-pull-structure (:count tgu/selection-trees) config/config nil [] {})
                           all-rules
                           user-db-id
                           nil)]
        (is (= (count (first actual-result)) 1))))

    (testing "multi uuid test"
      (let [patternV2 '(:db/id
                        :RAEmployee/name
                        :app/uuid
                        :app/active
                        {[:RARestaurant/_employees :as :RAEmployee/restaurants]
                         (:db/id
                          :app/active
                          :RARestaurant/name
                          :RARestaurant/subscriptionActive
                          {:RARestaurant/owner (:db/id :RAOwner/name)}
                          {[:RAIngredient/_restaurant :as :RARestaurant/ingredients]
                           (:db/id :app/active :RAIngredient/name :RAIngredient/archived)})})
            selection-treeV2 {:RAEmployee_list_output/count [nil],
                              :RAEmployee_list_output/value
                              [{:selections
                                {:RAEmployee/name [nil],
                                 :RAEmployee/uuid [nil],
                                 :app/active [nil],
                                 :RAEmployee/restaurants
                                 [{:selections
                                   {:app/active [nil],
                                    :RARestaurant/name [nil],
                                    :RARestaurant/subscriptionActive [nil]
                                    :RARestaurant/owner [{:selections {:RAOwner/name [nil]}}],
                                    :RARestaurant/ingredients
                                    [{:selections {:app/active [nil]
                                                   :RAIngredient/name [nil]
                                                   :RAIngredient/archived [nil]}}]}}]}}]}
            empls-uuids [#uuid "71389067-2137-123d-b624-8f78f880ad98" #uuid "7586a4e5-e3d2-4307-80e5-9544ba454db4"]
            filter-active (fn filter-active [m]
                            (cond (map? m) (->> m (map (fn [[k v]] [k (filter-active v)])) (into {}))
                                  (sequential? m) (vec (filter :app/active m))
                                  :else m))
            raw-db-data (q '[:find (pull ?e pattern)
                               :in $ [?uuid ...] pattern
                               :where [?e :app/uuid ?uuid]]
                             db-snapshot empls-uuids patternV2)
            expected-result (mapv (comp first filter-active) raw-db-data)
            raw-data (pull/perform-sequential-pull
                      config/config
                      nil
                      db-snapshot
                      (patterns/sequential-pull-structure selection-treeV2 config/config nil [] {})
                      all-rules
                      user-db-id
                      empls-uuids)
            actual-result (postwalk
                           (fn [m] (cond (= m empty-value) nil
                                         (map? m) (->> m
                                                       (keep (fn [[k v]]
                                                               (when (or (not (seqable? v))
                                                                         (seq v))
                                                                 [k v])))
                                                       (into {}))
                                         :else m))
                           raw-data)]
        (is (count-elements actual-result) (count-elements expected-result))
        (is (true? (deep-compare (map-vectors-to-sets actual-result) (map-vectors-to-sets expected-result))))))))
