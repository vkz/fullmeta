;; NOTE testing ground for CGI stuff

(ns main
  (:require [fullmeta.cgi                   :as cgi :refer [my their]]
            [fullmeta.prelude               :as fm.prelude :refer [make]]
            [clojure.tools.logging          :as log]
            [clojure.pprint                 :as pp]
            [clojure.string                 :as string]
            [ring.adapter.jetty             :as jetty]
            [ring.middleware.resource       :as mw.resource]
            [ring.middleware.file           :as mw.file]
            [ring.middleware.content-type   :as mw.content-type]
            [ring.middleware.params         :as mw.params]
            [ring.middleware.keyword-params :as mw.kwparams]
            [ring.util.response             :as ring.response]))


(declare handler trim-params)


(defrecord my-request-record [method identity store query-params form-params])
(def my-request my-request-record)
;; ----------------------------------------------- make cgi/my-request
(defmethod make my-request
  [_ obj & args]
  (apply make my-request :from obj args))

(defmethod make [my-request :from clojure.lang.IPersistentMap]
  [_ _ map]
  (map->my-request-record map))


(defn -main []
  (jetty/run-jetty
   ;; without actual path dispatch or below middleware we get such a cool error: any resource e.g.
   ;; /out/main.js script src will simply invoke our handler so suddenly main.js contents is
   ;; index.html contents. Obvious in retrospect of course.
   (-> handler
       ;; TODO should I add resources to classpath and serve public like this? If I don't the
       ;; wrap-file thing won't work if ever packaged in a jar. It is for actual filesystem.
       ;; (mw.resource/wrap-resource "public")
       ((cgi/middleware {:root "www"
                         :my {:request (partial make my-request)}}))
       (mw.file/wrap-file "resources/public")
       ;; NOTE even with (:my request) above we still rely on the wrap-params middleware cause
       ;; unless I'm mistaken it is the one that actually parses both form-params and query-params,
       ;; assocs them onto (:original request) and merges into its :params. If not, we maybe able to
       ;; replace it with our own that and we'll have to if ever we want to work with e.g. Nginx
       (mw.params/wrap-params)
       (mw.content-type/wrap-content-type))
   {:port 3000
    :join? false}))


(defmethod make [my-request :from cgi/request] [_ _ request]
  (make my-request :from
        {:method (-> request :original :request-method)
         :form-params (some-> request :original :form-params
                              (#'mw.kwparams/keyify-params :parse-namespaces!)
                              (trim-params))
         :query-params (some-> request :original :query-params
                               (#'mw.kwparams/keyify-params :parse-namespaces!)
                               (trim-params))}))


(defn handler [request]
  (println "RESOURCE NOT FOUND")
  (-> (ring.response/not-found (with-out-str
                                 (println "Can't find: " (:uri request))
                                 (println "-----------")
                                 (pp/pprint request)))
      (ring.response/content-type "text/plain")))


(defn trim-nilify [v] (some->> v (string/trim) (fm.prelude/nilify string/blank?)))
(defn trim-params [map] (reduce #(update %1 %2 trim-nilify) map (keys map)))
