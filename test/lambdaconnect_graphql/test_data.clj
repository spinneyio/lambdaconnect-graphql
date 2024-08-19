(ns lambdaconnect-graphql.test-data 
  (:require [lambdaconnect-graphql.utils :as u]))

(defn read-data
  [sourcefile]
  (read-string (slurp (str "resources/model/initial_data/" sourcefile))))

(defn initialize-global-data
  "Transacts data common for all users (allergens, units, vat).
   In the future, we should also add languages and translations here."
  [config]
  (let [{:keys [transact conn]} config
        now (u/now)
        allergens (read-data "test_data/allergens.edn")
        units (read-data "test_data/units.edn")
        vat (read-data "test_data/vat.edn")
        prices (->> (read-data "RAPrice.edn")
                    (map #(u/new-entity config "RAPrice" now % true)))
        subscription-products (->> (read-data "RASubscriptionProduct.edn")
                                   (map #(u/new-entity config "RASubscriptionProduct" now % true)))
        data-to-transact (concat allergens units vat prices subscription-products)]
    (transact conn data-to-transact)))

(defn initialize-new-user-data
  [internal-user-id email {first-name :firstName last-name :lastName referral :ref} config]
  (let [{:keys [q db conn transact]} config
        now (u/now)
        vat-value-id (->> (q '[:find ?v ?e
                               :where [?e :RAVatPercentage/percentage ?v]] (db conn))
                          (map (fn [[k v]] [(str k) v]))
                          (into {}))
        allegrens (flatten (q '[:find ?e :where [?e :RAAllergen/ident__]] (db conn)))
        unit-name-id (into {} (q '[:find ?v ?e
                                   :where [?e :RAUnit/name ?v]
                                   [?e :RAUnit/system "metric"]] (db conn)))
  
        owner (->> (read-data "RAOwner.edn")
                   (map #(u/new-entity config "RAOwner" now % true))
                   (map #(cond-> %
                           true (assoc :RAOwner/internalUserId internal-user-id
                                       :RAOwner/email email
                                       :RAOwner/name first-name
                                       :RAOwner/surname last-name)
                           (seq referral) (assoc :RAOwner/referral referral))))
        restaurant (map #(u/new-entity config "RARestaurant" now % true) (read-data "RARestaurant.edn"))
        categories (map #(u/new-entity config "RACategory" now % true) (read-data "RACategory.edn"))
        menus (map #(u/new-entity config "RAMenu" now % true) (read-data "RAMenu.edn"))
        ingredients (->> (read-data "RAIngredient.edn")
                         (map #(u/new-entity config "RAIngredient" now % true))
                         (map (fn [{:keys [RAIngredient/unit] :as ingredient}]
                                (assoc ingredient :RAIngredient/unit (get unit-name-id unit))))
                         (map-indexed (fn [idx ingredient]
                                        (if (= (mod idx 3) 0)
                                          (let [random-allergenes (->> allegrens
                                                                       (random-sample (/ 3 (max 3 (count allegrens)))))]
                                            (assoc ingredient :RAIngredient/allergens (vec random-allergenes)))
                                          ingredient))))
        recipes (->> (read-data "RARecipe.edn")
                     (map #(u/new-entity config "RARecipe" now % true))
                     (map (fn [{:keys [RARecipe/vatPercentage] :as recipe}]
                            (assoc recipe :RARecipe/vatPercentage (get vat-value-id vatPercentage)))))
        recipe-ingredients (->> (read-data "RARecipeIngredient.edn")
                                (map #(u/new-entity config "RARecipeIngredient" now % true))
                                (map (fn [{:keys [RARecipeIngredient/unit] :as ingredient}]
                                       (assoc ingredient :RARecipeIngredient/unit (get unit-name-id unit)))))
        sub-recipes (->> (read-data "RASubRecipe.edn")
                         (map #(u/new-entity config "RASubRecipe" now % true)))
        recipe-names ["kwasnica" "capriciosa" "margherita" "spaghetti_bolognese"
                      "goraca_czekolada" "puchar_lodowy" "szarlotka_na_goraco"
                      "ciasto_na_pizze"]
        recipe-preparation (->> recipe-names
                                (map (fn [recipe-name] (read-data (str "recipe_preparation/" recipe-name ".edn"))))
                                (map (fn [recipe-preparation] (map #(u/new-entity config "RARecipePreparation" now % true) recipe-preparation)))
                                flatten)
        data [owner restaurant categories menus ingredients recipes recipe-ingredients sub-recipes recipe-preparation]
        data-to-transact (->> (reduce concat data)
                              (map #(if (:db/id %) % (assoc % :db/id (str (:app/uuid %))))))]
    (transact conn data-to-transact)))

(defn initialize-data
  [{:keys [q conn transact db] :as config}]
  (let [internal-user-id (u/uuid)
        _ (transact conn [{:user/username "email@example.com"
                           :app/uuid internal-user-id}])
        _ (initialize-new-user-data internal-user-id "email@example.com" {:firstName "John" :lastName "Doe"} config)
        id (ffirst (q '[:find ?user :where [?user :user/username "email@example.com"]] (db conn)))]
    [id internal-user-id]))
