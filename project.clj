(defproject io.spinney/lambdaconnect-graphql "0.1.0"
  :description "Integrating model with GraphQL"
  :url "http://github.com/spinneyio/lambdaconnect-graphql"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.3"]
                 [failjure "2.2.0"]
                 [io.spinney/lambdaconnect-model "1.0.16"]]
  :repositories [["github" {:url          "https://maven.pkg.github.com/spinneyio/tiny-auth"
                            :username     "private-token"
                            :password      :env/GITHUB_TOKEN
                            :sign-releases false}]]
  :main ^:skip-aot lambdaconnect-graphql.core
  :target-path "target/%s"
  :profiles {:test {:plugins      [[com.jakemccrary/lein-test-refresh "0.24.1"]]
                    :dependencies [[mount "0.1.16"]
                                   [com.datomic/peer "1.0.7180"]
                                   [com.walmartlabs/lacinia "1.2.2"]
                                   [tiny-auth/tiny-auth "0.1.27"]]}})
