(ns lambdaconnect-graphql.test-sequential-data-pull
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.walk :refer [postwalk]]
            [lambdaconnect-graphql.core :refer [empty-value]]
            [lambdaconnect-graphql.test-config :as config]
            [lambdaconnect-graphql.test-data :refer [initialize-data]]))

(use-fixtures :each config/setup-test-environment)

(deftest ^:test-refresh/focus new-perform-sequential-pull-test
  (let [{:keys [db q conn user-rules-by-id] :as config} config/config
        [db-id] (initialize-data config)]
    (testing "pull custom by uuid"
      (let [names ["Przykładowy przepis gniazdo" "Spaghetti bolognese" "Puchar lodowy"]
            db (db conn)
            name->uuid (->> (q '[:find ?names ?v
                                 :in $ % ?user-db-id [?names ...]
                                 :where
                                 [?e :RARecipe/name ?names]
                                 (RARecipe ?user-db-id ?e)
                                 [?e :app/active true]
                                 [?e :app/uuid ?v]]
                               db
                               (user-rules-by-id nil db-id db)
                               db-id
                               names)
                            (into {}))
            uuids (map name->uuid names)
            result (:data (config/execute-query db-id (format "{RARecipe_by_uuid(uuid: [\"%s\", \"%s\", \"%s\"]) {uuid name}}" (nth uuids 0) (nth uuids 1) (nth uuids 2))))]
        (is result)
        (is (= names (map :name (:RARecipe_by_uuid result))))))
    (testing "pull custom all"
      (let [query "{RARecipe_all {count
                                  value {
                                    allergens {
                                      name
                                    }
                                    archived
                                    categories {
                                      name
                                    }
                                    createdAt
                                    grossPrice
                                    menus {
                                      name
                                    }
                                    name
                                    photo}}}"
            result (:RARecipe_all (:data (config/execute-query db-id query)))
            value-keys (map keyword '[allergens archived categories createdAt grossPrice menus name photo])]
        (is (= 10 (:count result)))
        (->> (:value result)
             (mapv (fn [entity] (let [entity-keys (keys entity)]
                                  (is (= (count entity-keys) (count value-keys)))
                                  (is (= (set entity-keys) (set value-keys)))))))))))

(deftest query-by-uuid-with-large-selection-tree
  (testing "Query by uuid with large selection tree"
    (let [[db-id] (initialize-data config/config)
          recipe-uuids (->> "{ RARecipe_all { value { uuid }}}"
                            (config/execute-query db-id)
                            :data
                            :RARecipe_all
                            :value
                            (map :uuid))
          test-repetitions 10
          recipe-uuids-samples (repeatedly test-repetitions #(random-sample 0.5 recipe-uuids))
          query (fn [recipe-uuids]
                  (format
                   "{
                    RARecipe_by_uuid(uuid: [%s]) {
                     uuid
                     name
                     grossPrice
                     subRecipes {
                       uuid
                       subRecipe {
                         name
                         grossPrice
                         allergens {
                           name
                           uuid
                         }
                       }
                       amount
                     }
                     menus {
                       uuid
                     }
                     photoUrl
                     photo
                     categories {
                       uuid
                       name
                     }
                     recipeIngredients {
                       ingredient {
                         calorificValue
                         amount
                         unit {
                           divisor
                           uuid
                           name
                           multiplier
                         }
                       }
                     }
                     numberOfPortionsToSell
                     numberOfPortionsProduced
                     allergens {
                       name
                       uuid
                     }
                   }
                 }" (str/join ", " (map #(format "\"%s\"" %) recipe-uuids))))]
      (doseq [recipe-uuids recipe-uuids-samples]
        (let [response (->> recipe-uuids query (config/execute-query db-id) :data :RARecipe_by_uuid)]
          (is (= (count recipe-uuids) (count response)))
          (let [has-empty-value (atom false)]
            (postwalk #(when (= % (str empty-value)) (reset! has-empty-value true)) response)
            (is (false? @has-empty-value))))))))
