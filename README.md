# lambdaconnect-graphql

GraphQL (integrated with Lacinia and Datomic) library.

TODO: Build status

## Instalation

```clojure
[io.spinney/lambdaconnect-graphql "0.1.2"]
```

## Features

TODO

## Integration

As a user of the library, you are interested in importing things defined in following files:
* `core.clj` - this is your main import source
* optionally `types.clj` and `utils.clj`

Spec should assist in plugging all the things together.

0. Create a `entities-by-name` using `lambdaconnect-model`.
1. Create a custom schema, using `core/custom-schema`. <br> Consult spec or examples to see how the schema parts should look like.
2. Create a GraphQL model, using `core/graphql-model` - args are:
   * `entities-by-name` from `lambdaconnect-model`,
   * `custom-schema` - created in step 1.,
   * `deprecated-fields` - map from namespaced attribute to string
3. Create a config using `core/lambdaconnect-graphql-config`. Consult spec or examples for explanation of the args.
4. Create resolvers and interceptors, using functions `core/resolvers`, `core/db-grapqhl-interceptor`, `core/user-grapqhl-interceptor` and `core/audit-grapqhl-interceptor`.
5. Compile schema: <br>
```
(-> graphql-model ;; Created in step 2.
    (com.walmartlabs.lacinia.util/attach-resolvers resolvers)
    com.walmartlabs.lacinia.schema/compile)
```
6. Insert GraphQL endpoint(s) into Pedestal service
```
(:require [com.walmartlabs.lacinia.pedestal2 :as lp2]
          [io.pedestal.http.route :as route]
          [io.pedestal.http :as server])

(defn graphql-main-interceptors [model]
  ;; Interceptors created in step 4.
  (into [...
         db-grapqhl-interceptor
         ...
         user-graphql-interceptor
         ...
         audit-graphql-interceptor
         ...]
        (lp2/default-interceptors model nil)))

(def ^:private default-asset-path "/assets/graphiql")
(def ^:private options {:api-path "/graphql" :ide-path "/graphiql"})

(defn graphql-interceptors [model]
 (let [interceptors (graphql-main-interceptors model)]
   (route/expand-routes
    (into #{[(:api-path options) :post interceptors :route-name ::graphql-api]
            [(:ide-path options) :get (lp2/graphiql-ide-handler options) :route-name ::graphiql-ide]}
          (lp2/graphiql-asset-routes default-asset-path)))))
          
(defn attach-graphql
  [routes]
  (into (routes) (graphql-interceptors graphql/schema))) ;; Schema created in step 5.

;; In your own service namespace
(defn create-server []
  (let [server (-> service/service ;; your own service with all endpoints etc.
                   (assoc ::server/port (:port env))
                   (update ::server/routes attach-graphql)
                   lp2/enable-graphiql
                   (server/default-interceptors)
                   server/create-server)]
    server))
```


## License

Copyright © 2024 Spinney

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
