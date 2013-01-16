(ns flow-gl.applications.datomic
  (:require (flow-gl.gui [drawable :as drawable]
                         [layout :as layout]
                         [view :as view]
                         [application :as application])

            (flow-gl [dataflow :as dataflow]
                     [debug :as debug]))
  (:use [datomic.api :only [q db] :as d]))


(comment

  ;; store database uri
  (def uri "datomic:mem://seattle")

  ;; create database
  (d/create-database uri)
  (d/delete-database uri)

  ;; connect to database
  (def conn (d/connect uri))

  (def schema [ {:db/id #db/id[:db.part/db]
                 :db/ident :person/name
                 :db/valueType :db.type/string
                 :db/cardinality :db.cardinality/one
                 :db/fulltext true
                 :db.install/_attribute :db.part/db}

                {:db/id #db/id[:db.part/db]
                 :db/ident :person/frends
                 :db/valueType :db.type/ref
                 :db/cardinality :db.cardinality/many
                 :db.install/_attribute :db.part/db}
                ])

  (d/transact conn schema)

  (def data [{:person/name "Pekka" :db/id #db/id[:db.part/user -1000001], :person/frends [#db/id[:db.part/user -1000002]]}
             {:person/name "Jaakko" :db/id #db/id[:db.part/user -1000002]}])

  (d/transact conn data)



  (def results (q '[:find ?person ?name :where [?person :person/name ?name]] (db conn)))

  (for [result results]
    (println result))

(def entity
    (d/entity (db conn)
              (first (first results))))

(:person/name entity)
(:db/ident entity)
(-> entity :person/frends first :person/name)


  (type )
  )
