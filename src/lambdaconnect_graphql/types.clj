(ns lambdaconnect-graphql.types)

(def types-map '{:db.type/string String
                 :db.type/instant String
                 :db.type/boolean Boolean
                 :db.type/long Int
                 :db.type/float Float
                 :db.type/double Float
                 :db.type/uuid ID})

(def inverse-types-map '{String :db.type/string
                         Boolean :db.type/boolean
                         Int :db.type/long
                         Float :db.type/double
                         ID :db.type/uuid})
