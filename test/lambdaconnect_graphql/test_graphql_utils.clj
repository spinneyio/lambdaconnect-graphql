(ns lambdaconnect-graphql.test-graphql-utils
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [lambdaconnect-graphql.patterns :as patterns]
            [lambdaconnect-graphql.test-config :as config]))

(use-fixtures :each config/setup-test-environment)

(def selection-trees
  {:count-value {:RAOwner_list_output/count [nil],
                 :RAOwner_list_output/value
                 [{:selections
                   {:RAOwner/name [nil],
                    :RAOwner/surname [nil],
                    :RAOwner/employees
                    [{:selections
                      {:RAEmployee/name [nil],
                       :RAEmployee/restaurants
                       [{:selections
                         {:RARestaurant/name [nil],
                          :RARestaurant/owner [{:selections {:RAOwner/name [nil]}}],
                          :RARestaurant/ingredients
                          [{:selections {:RAIngredient/name [nil]}}]}}]}}]}}]}

   :count {:RAOwner_list_output/count [nil]}

   :value {:RAOwner_list_output/value
           [{:selections
             {:RAOwner/name [nil],
              :RAOwner/surname [nil],
              :RAOwner/employees
              [{:selections
                {:RAEmployee/name [nil]}}]}}]}
   :customs [{:RARecipe_list_output/count [nil],
              :RARecipe_list_output/value
              [{:selections
                {:RARecipe/recipeIngredients
                 [{:selections
                   {:RARecipeIngredient/amount [nil],
                    :RARecipeIngredient/ingredient
                    [{:selections
                      {:RAIngredient/name [nil],
                       :RAIngredient/calorificValue [nil]}}]}}],
                 :RARecipe/allergens [{:selections {:RAAllergen/name [nil]}}],
                 :RARecipe/name [nil],
                 :RARecipe/categories
                 [{:selections {:RACategory/uuid [nil], :RACategory/name [nil]}}],
                 :RARecipe/photoUrl [nil]}}]}
             {:RAAllergen_list_output/count [nil],
              :RAAllergen_list_output/value
              [{:selections
                {:RAAllergen/name [nil],
                 :RAAllergen/recipes
                 [{:selections {:RARecipe/name [nil]}}]}}]}]})

(deftest sequential-pull-structure-test
  (let [parse-relationship (fn [relationship] (mapv (fn [[rel-id rel-value]] [(keyword (namespace rel-id)) rel-value]) relationship))
        checking-function (fn [actual expected]
                            (->> actual
                                 (mapv
                                  (fn [[actual-key [actual-normal-val actual-custom-val]]]
                                    (is (and actual-normal-val actual-custom-val))
                                    (let [compare-result (let [parsed-actual-val [(update actual-normal-val :relationships parse-relationship)
                                                                                  (update actual-custom-val :relationships parse-relationship)]]
                                                           (->> expected
                                                                (mapv (fn [[expected-key expected-value]]
                                                                        (and
                                                                         (= parsed-actual-val expected-value)
                                                                         (= (namespace actual-key) (namespace expected-key)))))))
                                          is-match? (some true? compare-result)]
                                      (is is-match? (str "actual: "
                                                         [(update actual-normal-val :relationships parse-relationship)
                                                          (update actual-custom-val :relationships parse-relationship)]
                                                         "\nwas not found in expected: " expected)))))))]

    (testing "value & count selection tree"
      (let [expected-result
            {:RAOwner/first
             [{:relationships [[:RAEmployee {[:RAEmployee/_owner :as :RAOwner/employees] [:db/id]}]],
               :attributes [:db/id :RAOwner/name :RAOwner/surname]}
              {:relationships [], :attributes []}],
             :RAEmployee/first
             [{:relationships [[:RARestaurant {[:RARestaurant/_employees :as :RAEmployee/restaurants] [:db/id]}]],
               :attributes [:db/id :RAEmployee/name]}
              {:relationships [], :attributes []}],
             :RARestaurant/first
             [{:relationships
               [[:RAOwner {:RARestaurant/owner [:db/id]}]
                [:RAIngredient {[:RAIngredient/_restaurant :as :RARestaurant/ingredients] [:db/id]}]],
               :attributes [:db/id :RARestaurant/name]}
              {:relationships [], :attributes []}],
             :RAOwner/second [{:relationships [], :attributes [:db/id :RAOwner/name]} {:relationships [], :attributes []}],
             :RAIngredient/first
             [{:relationships [], :attributes [:db/id :RAIngredient/name]} {:relationships [], :attributes []}]}

            expected-execution-order [:RAOwner :RAEmployee :RARestaurant :RAOwner :RAIngredient]

            [actual-pull-structures actual-execution-order] (patterns/sequential-pull-structure (:count-value selection-trees) config/config nil [] {})
            actual-execution-order-parsed (mapv (fn [{:keys [entity-id]}] (keyword (namespace entity-id))) actual-execution-order)]
        (is (= 5 (count (keys actual-pull-structures))))
        (checking-function actual-pull-structures expected-result)
        (is (= actual-execution-order-parsed expected-execution-order))
        (is (and (not= (get actual-execution-order 0) (get actual-execution-order 3))
                 (= (keyword (namespace (:entity-id (get actual-execution-order 0))))
                    (keyword (namespace (:entity-id (get actual-execution-order 3)))))))))

    (testing "count only selection tree"
      (let [expected-result {:RAOwner/first [{:relationships [], :attributes [:db/id]} {:relationships [], :attributes []}]}
            expected-execution-order [:RAOwner]
            [actual-pull-structures actual-execution-order] (patterns/sequential-pull-structure (:count selection-trees) config/config nil [] {})
            actual-execution-order-parsed (mapv (fn [{:keys [entity-id]}] (keyword (namespace entity-id))) actual-execution-order)]
        (is (= 1 (count (keys actual-pull-structures))))
        (checking-function actual-pull-structures expected-result)
        (is (= actual-execution-order-parsed expected-execution-order))))

    (testing "value only selection tree"
      (let [expected-result
            {:RAOwner/first
             [{:relationships [[:RAEmployee {[:RAEmployee/_owner :as :RAOwner/employees] [:db/id]}]],
               :attributes [:db/id :RAOwner/name :RAOwner/surname]}
              {:relationships [], :attributes []}],
             :RAEmployee/first [{:relationships [], :attributes [:db/id :RAEmployee/name]} {:relationships [], :attributes []}]}
            expected-execution-order [:RAOwner :RAEmployee]

            [actual-pull-structures actual-execution-order] (patterns/sequential-pull-structure (:value selection-trees) config/config nil [] {})
            actual-execution-order-parsed (mapv (fn [{:keys [entity-id]}] (keyword (namespace entity-id))) actual-execution-order)]
        (is (= 2 (count (keys actual-pull-structures))))
        (checking-function actual-pull-structures expected-result)
        (is (= actual-execution-order-parsed expected-execution-order))))

    (testing "custom attr and relationships"
      (let [expected-result
            {:RARecipe/G__64712
             [{:relationships
               [[:RARecipeIngredient {[:RARecipeIngredient/_recipe :as :RARecipe/recipeIngredients] [:db/id]}]
                [:RACategory {:RARecipe/categories [:db/id]}]],
               :attributes [:db/id :RARecipe/name]}
              {:relationships [[:RAAllergen :RARecipe/allergens]],
               :attributes
               [:RARecipe/photoUrl]}],
             :RARecipeIngredient/G__64713
             [{:relationships [[:RAIngredient {:RARecipeIngredient/ingredient [:db/id]}]],
               :attributes [:db/id :RARecipeIngredient/amount]}
              {:relationships [], :attributes []}],
             :RAIngredient/G__64714
             [{:relationships [], :attributes [:db/id :RAIngredient/name :RAIngredient/calorificValue]}
              {:relationships [], :attributes []}],
             :RAAllergen/G__64715 [{:relationships [], :attributes [:db/id :RAAllergen/name]} {:relationships [], :attributes []}],
             :RACategory/G__64716
             [{:relationships [], :attributes [:db/id :app/uuid :RACategory/name]} {:relationships [], :attributes []}]}
            expected-execution-order [:RARecipe :RARecipeIngredient :RAIngredient :RAAllergen :RACategory]
            [actual-pull-structures actual-execution-order] (patterns/sequential-pull-structure (first (:customs selection-trees)) config/config nil [] {})
            actual-execution-order-parsed (mapv (fn [{:keys [entity-id]}] (keyword (namespace entity-id))) actual-execution-order)]
        (is (= 5 (count (keys actual-pull-structures))))
        (checking-function actual-pull-structures expected-result)
        (is (= actual-execution-order-parsed expected-execution-order))))))
