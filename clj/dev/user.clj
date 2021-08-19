(ns user
  (:require [prelude :as dev.prelude]
            [main]
            [clojure.edn :as edn]
            [clojure.tools.logging :as log]
            [clojure.tools.namespace.repl :refer [refresh] :as tools.ns]
            [clojure.tools.deps.alpha :as tools.deps]))

(defn dirs-allowed-to-refresh
  "Classpath trimmed of dirs under :do-not-refresh key in :dev alias in
  deps.edn"
  []
  (let [do-not-refresh (->> (dev.prelude/deps-edn)
                            :custom :do-not-refresh
                            (into #{}))]
    (->> (dev.prelude/classpath)
         ;; turn them into path strings
         (map str)
         ;; filter out do-not-refresh dirs
         (filter (complement do-not-refresh)))))

;; Set dirs allowed to be refreshed (also effects cider-ns-refresh)
(apply tools.ns/set-refresh-dirs (dirs-allowed-to-refresh))
;; just in case, but we shouldn't need this
;; (tools.ns/disable-reload!)
;; (meta *ns*)
;; (alter-meta! *ns* dissoc :clojure.tools.namespace.repl/load)

;; NOTE to understand why see:
;; https://github.com/clojure/tools.namespace#warnings-for-helper-functions
(def server nil)
(defn start []
  (alter-var-root #'server (constantly
                            (let [server (main/-main)]
                              (when server
                                (println "[user] server started")
                                server)))))
(defn stop []
  (when server (.stop server) (println "[user] server stopped"))
  (alter-var-root #'server (constantly nil)))
(defn restart []
  (println "[user] refresh and restart ...")
  (stop)
  ;; TODO refresh fails if ever you remove or rename a clj file. Cause I think it caches that module
  ;; and attempts to reload it from path that no longer exists. This is rare but with our CGI
  ;; modules coming and going it happens on occasion.
  (tools.ns/refresh :after 'user/start))


#_
(defonce dev-server
  (do (start) server))


;; TODO set dir-locas:
;;
;;   cider-ns-refresh-before-fn "user/stop"
;;   cider-ns-refresh-after-fn "user/start"
;;
;; after that it'll be safe to call cider-ns-refresh. It'll be
;; equivalent to calling restart or reset for the system.


(defn browser-reload [& {f :after}]
  (log/info "Reloading browser window ... stand by"))


(defn -main [& args]
  (when-not server
    (restart)))

(comment
  ;; from CLJS
  #_(.toUTCRfc3339String (goog.date.UtcDateTime.))
  ;; =>
  "2020-09-15T09:15:18.869Z"
  ;; to CLJ
  (java.time.Instant/parse "2020-09-15T09:15:18.869Z")

  (java.time.LocalDateTime/of (java.util.Date.))
  (java.time.LocalDateTime/ofInstant (java.time.Instant/now)
                                     (java.time.ZoneId/of "UTC"))
  (java.time.LocalDateTime/ofInstant (java.time.Instant/now)
                                     (java.time.ZoneId/systemDefault))
  (java.time.ZoneId/systemDefault)
  (java.time.ZoneOffset/systemDefault)
  (java.time.ZoneId/getAvailableZoneIds)
  (java.time.ZoneId/of "UTC")
  (java.time.LocalDateTime/now)
  (java.time.ZonedDateTime/now)
  (java.time.OffsetDateTime/now)

  ;; Datomic's and therefore Datalevin (and probably Datascript) :db.type/insntant is Value type for
  ;; instants in time. Stored internally as a number of milliseconds since midnight, January 1, 1970
  ;; UTC. Maps to java.util.Date on Java platforms. Neither java.util.Date nor java.time.Instant
  ;; appear to automatically cast when inserted, so we have to:
  (java.time.Instant/ofEpochMilli (.toEpochMilli (java.time.Instant/now)))


  ;; java.util.Date can be converted into java.time
  (.toInstant (java.util.Date.))
  ;; comment
  )


(comment
  (require '[datascript.core :as d])

  (def schema {:aka {:db/cardinality :db.cardinality/many}})
  (def conn (d/create-conn schema))
  (d/transact! conn [{:db/id -1
                      :name "Maksim"
                      :age 45
                      :aka ["Max Otto von Stierlitz", "Jack Ryan"]}])
  (d/transact! conn [{:db/id -1
                      :name "Oleg"
                      :age 50
                      :aka ["Karl", "Marx"]}])
  (defn all [conn]
    (doall
     (d/q '[:find [(pull ?e [*]) ...]
            :in $
            :where [?e ?a ?v]]
          @conn)))

  #_
  (all conn)
  #_
  (d/datoms @conn :eavt)

  (def conn2 (-> @conn
                 (d/datoms :eavt)
                 (d/conn-from-datoms schema)))
  (d/datoms @conn2 :eavt)
  (d/transact! conn2 [{:db/id -1
                       :name "Mark"
                       :age 15
                       :aka ["Bash"]}])
  (->> (d/datoms @conn :eavt)
       (pr-str)
       (edn/read-string {:readers {'datascript/Datom datascript.db/datom-from-reader
                                   'datascript/DB datascript.db/db-from-reader}})
       #_((comp d/datom? first)))

  (def conn3
    (->> @conn
         (pr-str)
         (edn/read-string {:readers {'datascript/Datom datascript.db/datom-from-reader
                                     'datascript/DB datascript.db/db-from-reader}})
         (d/conn-from-db)))
  (d/datoms @conn3 :eavt)
  (d/transact! conn3 [{:db/id -1
                       :name "Vlad"
                       :age 38
                       :aka ["FM"]}])
  (all conn3)

  ;; https://github.com/tonsky/datascript/wiki/Tips-&-tricks#edn-serialization
  (def conn4
    (->> (d/db conn)
         (pr-str)
         (edn/read-string {:readers datascript.core/data-readers})
         (d/conn-from-db)))

  (= (all conn4)
     (all conn))

  (d/touch (d/entity (d/db conn) 1))
  (d/touch (d/entity @conn 1))
  ;; comment
  )
