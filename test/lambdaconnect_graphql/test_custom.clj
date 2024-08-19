(ns lambdaconnect-graphql.test-custom 
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [datomic.api :as d]
            [lambdaconnect-model.core :as lm]))

(def basic-schema
  [{:db/ident              :app/uuid
    :db/valueType          :db.type/uuid
    :db/cardinality        :db.cardinality/one
    :db/unique             :db.unique/identity
    :db/doc                "UUID for an object in the system"}

   {:db/ident              :app/createdAt
    :db/valueType          :db.type/instant
    :db/cardinality        :db.cardinality/one
    :db/doc                "Creation time"}

   {:db/ident              :app/updatedAt
    :db/valueType          :db.type/instant
    :db/cardinality        :db.cardinality/one
    :db/doc                "Last update time"}

   {:db/ident              :app/active
    :db/valueType          :db.type/boolean
    :db/cardinality        :db.cardinality/one
    :db/doc                "If false, it means the entity was deleted"}

   {:db/ident              :audit/ip-address
    :db/valueType          :db.type/string
    :db/cardinality        :db.cardinality/one
    :db/doc                "Transaction audit - ip"}

   {:db/ident              :audit/user
    :db/valueType          :db.type/ref
    :db/cardinality        :db.cardinality/one
    :db/doc                "Transaction audit - user"}

   {:db/ident              :audit/http-method
    :db/valueType          :db.type/string
    :db/cardinality        :db.cardinality/one
    :db/doc                "Transaction audit - method"}

   {:db/ident              :audit/http-path
    :db/valueType          :db.type/string
    :db/cardinality        :db.cardinality/one
    :db/doc                "Transaction audit - path"}

   {:db/ident              :audit/http-agent
    :db/valueType          :db.type/string
    :db/cardinality        :db.cardinality/one
    :db/doc                "Transaction audit - user-agent"}])

(def custom-input-objects-fields
  '{:RARecipe_input
    {:fields
     {:deleteWithRecipeIngredients {:type Boolean
                                    :description "Set this field to true to deactivate this recipe and all RARecipeIngredients
                                                     with recipe field set to this."}}}})

(def custom-object-fields
  '{:RARecipe
    {:fields
     {:photoUrl {:type String
                 :description "Presigned photo URL"}
      :allergens {:type (list RAAllergen)
                  :description "All allergens appearing in recipe ingredients (including subrecipes)."}
      :ingredientsNested {:type (list RAIngredient)
                          :description "All ingredients used either by this recipe or subrecipes (nested at any depth)."}}}
    :RAAllergen
    {:fields
     {:recipes {:type (list RARecipe)
                :description "All recipes containing that allergen."}}}
    :RAIngredient
    {:fields
     {:nestedRecipesUsingIngredient
      {:type (list RARecipe)
       :description "All recipes using that ingredient, directly or indirectly (through subrecipe)."}}}})

(def deprecated-fields
  {:RAUnit/multiplier "Use divisor instead"})

(defn RAIngredient-nestedRecipesUsingIngredient-fn
  [{:keys [db ids rules scoped-ids]}]
  (->> (d/q '[:find ?ingredient (set ?recipe)
              :in $ % [?ingredient ...]
              :where
              [?recipe-ingredient :RARecipeIngredient/ingredient ?ingredient]
              [?recipe-ingredient :app/active true]
              [?recipe-ingredient :RARecipeIngredient/recipe ?subrecipe]
              [?subrecipe :app/active true]
              (RARecipe-reachable-backward ?recipe ?subrecipe)] db rules ids)
       (map (fn [[id recipes]] [id (set/intersection @(scoped-ids "RARecipe") recipes)]))
       (into {})))

(defn RARecipe-ingredientsNested-fn
  [{:keys [db ids rules scoped-ids]}]
  (->> (d/q '[:find ?recipe (set ?ingredient)
              :in $ % [?recipe ...]
              :where
              (RARecipe-reachable-forward ?recipe ?reachable-recipe)
              [?recipe-ingredient :RARecipeIngredient/recipe ?reachable-recipe]
              [?recipe-ingredient :app/active true]
              [?recipe-ingredient :RARecipeIngredient/ingredient ?ingredient]
              [?ingredient :app/active true]] db rules ids)
       (map (fn [[id ingredients]] [id (set/intersection @(scoped-ids "RAIngredient") ingredients)]))
       (into {})))

(def answer-key-fns {:RARecipe/photoUrl (fn [s] (when s (str s s)))})

(def custom-field-verifiers {:RARecipe/grossPrice (fn [price] (when (odd? price) "Price must be even!"))})

(def custom-attributes
  {:RARecipe/photoUrl
   {:type :db.type/string
    :cardinality :db.cardinality/one
    :fn (fn [{:keys [db ids]}]
          (->> (d/q '[:find ?recipe ?photo
                      :in $ [?recipe ...]
                      :where [?recipe :RARecipe/photo ?photo]]
                    db ids)
               (into {})))}})

(def custom-relationships
  "Custom relationships rules should not resolve with empty value and should use scoping rules inside them.
   Custom relationships functions should resolve with nil where appropriate and should scopre results with supplied scoped-ids."
  {:RAAllergen/recipes
   {:to-many true
    :inverse-entity "RARecipe"
    :inverse-relationship :RARecipe/allergens
    :fn (fn [{:keys [db ids scoped-ids] :as args}]
          (let [allergen->ingredients (->> (d/q '[:find ?allergen (set ?ingredient)
                                                  :in $ [?allergen ...]
                                                  :where [?ingredient :RAIngredient/allergens ?allergen]]
                                                db ids)
                                           (map (fn [[id ingredients]]
                                                  [id (set/intersection ingredients @(scoped-ids "RAIngredient"))]))
                                           (into {}))
                ingredient->recipes (RAIngredient-nestedRecipesUsingIngredient-fn (assoc args :ids (apply set/union (vals allergen->ingredients))))]
            (->> allergen->ingredients
                 (map (fn [[id ingredients]] [id (apply set/union (map ingredient->recipes ingredients))]))
                 (into {}))))}


   :RAIngredient/nestedRecipesUsingIngredient
   {:to-many true
    :inverse-entity "RARecipe"
    :inverse-relationship :RARecipe/ingredientsNested
    :fn RAIngredient-nestedRecipesUsingIngredient-fn}


   :RARecipe/ingredientsNested
   {:to-many true
    :inverse-entity "RAIngredient"
    :inverse-relationship :RAIngredient/nestedRecipesUsingIngredient
    :fn RARecipe-ingredientsNested-fn}

   :RARecipe/allergens
   {:to-many true
    :inverse-entity "RAAllergen"
    :inverse-relationship :RAAllergen/recipes
    :fn (fn [{:keys [db scoped-ids] :as args}]
          (let [recipe->ingredients (RARecipe-ingredientsNested-fn args)
                ingredient->allergens (->> recipe->ingredients
                                           vals
                                           (apply set/union)
                                           (d/q '[:find ?ingredient (set ?allergen)
                                                  :in $ [?ingredient ...]
                                                  :where [?ingredient :RAIngredient/allergens ?allergen]]
                                                db)
                                           (map (fn [[ingredient allergens]] [ingredient (set/intersection allergens @(scoped-ids "RAAllergen"))]))
                                           (into {}))]
            (->> recipe->ingredients
                 (map (fn [[id ingredients]] [id (apply set/union (map ingredient->allergens ingredients))]))
                 (into {}))))}})

(def custom-db-verifiers
  {"uniquePrices"
   (fn [{:keys [flat-parsed-input initial-db]}]
     (let [new-prices (set (keep (fn [[entity-name value]]
                                   (when (= entity-name "RARecipe")
                                     (:RARecipe/grossPrice value)))
                                 flat-parsed-input))
           old-prices (set (map first (d/q '[:find ?grossPrice
                                             :where [_ :RARecipe/grossPrice ?grossPrice]]
                                           initial-db)))]
       (when (seq (set/intersection new-prices old-prices))
         "Recipe gross prices must be unique!")))})

(def custom-transaction-modifiers
  [["refuseOddPriceChanges"
    (fn [{:keys [transaction]}]
      {:transaction (remove (fn [row]
                              (when (sequential? row)
                                (let [[db-action eid attribute value] row]
                                  (when (and (= :db/add db-action)
                                             (sequential? eid)
                                             (= :RARecipe/grossPrice attribute)
                                             (odd? value))
                                    "remove"))))
                            transaction)})]])

(defn get-scoped-variable
  "Input:
   scoping-query
   Output:
   scoped-variable - variable which to which scoped entities are bound in query given"
  [scoping-query]
  (second scoping-query))

(defn get-entity-rules
  "Returnes list of rules concerning an entity to which tags-referring-to-entity refer to.
   Input:
   entity-name,
   tags-referring-to-entity - scoping queries of tags referring to an entity

   Output:
   [[(entity-name [?user ?id-found]) [?user :app/uuid (gensym)] [clause-1-of-rule0] ... [(= ?scoped-variable ?id-found)]]
   [(entity-name [?user ?id-found]) [?user :app/uuid (gensym)] [clause-1-of-rule1] ... [(= ?scoped-variable ?id-found)]]]

   ?user - user to whom the query is scoped,
   ?id-found - id of entity found by query in which scoping is used"

  [entity-name tags-referencing-entity]
  (->> tags-referencing-entity
       (mapv (fn [[_ standard-query]]
               (let [scoped-entity-variable (get-scoped-variable standard-query)
                     scoped-user-variable '?user
                     standard-rule-header `(~(symbol entity-name) ~scoped-user-variable ~scoped-entity-variable)
                     standard-where-clauses (rest (drop-while #(not= :where %) standard-query))]
                 (vec (concat [standard-rule-header] standard-where-clauses)))))))

(defn get-scoping-rules
  [k-pull-scoping-edn entities-by-name]
  (let [scoping-queries (lm/get-scoping-queries entities-by-name k-pull-scoping-edn false)
        entity? (fn [pattern string]
                  (re-find pattern string))
        entities (keys entities-by-name)
        get-rules-per-entity (fn [entity-name]
                               (let [tags-referencing-entity (filter (fn [[tag _]]
                                                                       (entity? (re-pattern (str entity-name "\\.")) (str tag)))
                                                                     scoping-queries)]
                                 (get-entity-rules entity-name tags-referencing-entity)))
        all-rules (mapcat get-rules-per-entity entities)]
    all-rules))

(defn multiple-steps-relation
  "Input: input-relation (definition of A)
          output-relation-symbol (B)
          depths (map of non-negative ints)
          variable-name (used for readability of generated rules)
          reverse? (is rule computed ?x->?y or ?x<-?y)
   Output: relation B = sum A^i where i in depths"
  ([input-relation
    output-relation-symbol
    depths
    variable-name
    reverse?]
   (let [input-relation-symbol (ffirst input-relation)
         sym (fn [s] (symbol (str "?" variable-name "-" s)))
         [beg end] (map sym ["beg" "end"])
         single-depth-body
         (fn [i]
           (if (zero? i)
             (into [(list output-relation-symbol beg end)]
                   [(if reverse?
                      [(list 'identity end) beg]
                      [(list 'identity beg) end])])
             (into [(list output-relation-symbol (sym 0) (sym i))]
                   (->> i range
                        (map (fn [j] (list input-relation-symbol (sym j) (sym (inc j)))))
                        ((if reverse? reverse identity))))))]
     (into [input-relation]
           (mapv single-depth-body depths)))))

(def max-recipes-depth 7)

(def recipe-edge-forward
  '[(RARecipe-edge-forward ?recipe1 ?recipe2)
    [?subrecipe :RASubRecipe/recipe ?recipe1]
    [?subrecipe :app/active true]
    [?subrecipe :RASubRecipe/subRecipe ?recipe2]
    [?recipe2 :app/active true]
    [?recipe2 :RARecipe/archived false]])

(defn reachable-recipes
  [edge-rule path-rule-symbol reverse?]
  (into [edge-rule]
        (multiple-steps-relation
         edge-rule
         path-rule-symbol
         (range (inc max-recipes-depth))
         "recipe"
         reverse?)))

(def forward-reachable-recipes
  (reachable-recipes
   recipe-edge-forward
   'RARecipe-reachable-forward
   false))

(def too-large-recipes-chain
  (multiple-steps-relation
   recipe-edge-forward
   'RARecipe-reachable-forward-over-limit
   [(inc max-recipes-depth)]
   "recipe"
   false))

;; These rules return the same answers as
;; forward-reachable-recipes, but the clauses are changed
;; to efficiently walk on edges in reverse direction
;; (subrecipes to recipes instead of recipes to subrecipes)
(def backward-reachable-recipes
  (reachable-recipes
   '[(RARecipe-edge-backward ?recipe1 ?recipe2)
     [?subrecipe :RASubRecipe/subRecipe ?recipe2]
     [?subrecipe :app/active true]
     [?subrecipe :RASubRecipe/recipe ?recipe1]
     [?recipe1 :app/active true]
     [?recipe1 :RARecipe/archived false]]
   'RARecipe-reachable-backward
   true))

(def recipe-base-restaurant
  '[[(RARecipe-base-RARestaurant ?recipe ?restaurant)
     (RARecipe-reachable-forward ?recipe ?reachable-recipe)
     (or-join [?reachable-recipe ?restaurant]
              (and [?reachable-recipe :RARecipe/menus ?menu]
                   [?menu :RAMenu/restaurant ?restaurant])
              (and [?reachable-recipe :RARecipe/categories ?category]
                   [?category :RACategory/restaurant ?restaurant])
              (and [?recipe-ingredient :RARecipeIngredient/recipe ?reachable-recipe]
                   [?recipe-ingredient :RARecipeIngredient/ingredient ?ingredient]
                   [?ingredient :RAIngredient/restaurant ?restaurant]))]])

(def custom-rules
  (vec (concat forward-reachable-recipes
               backward-reachable-recipes
               too-large-recipes-chain
               recipe-base-restaurant)))

(defn get-user-scoping-permissions
  [resource-path]
  (let [rules (map (fn [[rule-name rule-info]]
                     [(-> rule-name name (str/split #"\.") first) (:permissions rule-info)])
                   (read-string (slurp resource-path)))]
    (assert (->> rules
                 (group-by first)
                 (keep (fn [[entity rules]]
                         (when (->> rules
                                    (map second)
                                    set
                                    count
                                    (not= 1))
                           entity)))
                 empty?)
            (str "Contradicting permissions in " resource-path "!"))
    (into {} rules)))
