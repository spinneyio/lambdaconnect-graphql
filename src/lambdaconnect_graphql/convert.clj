(ns lambdaconnect-graphql.convert 
  (:require [clojure.walk :refer [postwalk]]
            [lambdaconnect-graphql.utils :as u]))

(defn format-answer
  [m]
  (postwalk (fn [k] (cond (and (keyword? k) (namespace k)) (keyword (name k))
                          (uuid? k) (str k)
                          (= (class k) java.util.Date) (u/format-date k)
                          :else k)) m))

(defn update-answer-keys
  [m {:keys [answer-key-fns]}]
  (postwalk (fn [ans] (if (map? ans)
                        (reduce (fn [ans [ans-key key-fn]]
                                  (cond-> ans
                                    (contains? ans ans-key)
                                    (update ans-key key-fn)))
                                ans answer-key-fns)
                        ans)) m))

(defn db-convert
  "Convert response from Datomic to be digestible by Lacinia.
   Currently, transformations are:
   * transformations specified in custom map answer-key-fns
   * remove namespaces in keywords and transform uuids to string
   * stringify date by using custom function instead of str"
  [config answer]
  (when answer
    (-> answer
        (update-answer-keys config)
        format-answer)))
