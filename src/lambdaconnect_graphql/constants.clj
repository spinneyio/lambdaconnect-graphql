(ns lambdaconnect-graphql.constants)

;; These two variables should not be redefined unless you are testing or you know what you're doing!
(def ^:dynamic *ignore-mutation-scoping?* false)
(def ^:dynamic *ignore-obligatory-relations?* false)

(def empty-value
  "Datomic does not resolve rule if nil is returned, so we use a temporary value
   that we have to later replace manually with nil."
  ::empty-value)
