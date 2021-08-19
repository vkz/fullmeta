(ns fullmeta.test.www1
  (:require [fullmeta.cgi :as cgi]))

(alias 'www1 'fullmeta.test.www1)

;; assertion fail => throw
(defn ^:cgi a [request]
  (cgi/assert (not true))
  [`a (-> request :path :unmatched)])

;; assertion fail => [42 unmatched-path]
(defn ^:cgi b [request]
  (cgi/assert (not true) (constantly [42 (-> request :path :unmatched)]))
  [`b (-> request :path :unmatched)])

(def next-handler (constantly nil))

;; assertion fail => next-handler => [`cgi unmatched-path]
(defn ^:cgi c [request]
  (cgi/assert (not true) next-handler)
  [`c (-> request :path :unmatched)])

(defn auth-owner [request]
  (if (some-> request :path :unmatched (->> (some #{"owner"})))
    :authorized
    (cgi/set-condition! :unauthorized)))

;; assertion fail => set cgi/*condition* => show cgi/*condition*
;; assertion succeed => [`d ("owner")]
(defn ^:cgi d [request]
  (cgi/assert (auth-owner request) (fn [assertion] cgi/*condition*))
  [`d (-> request :path :unmatched)])

(defn ^:cgi e [request]
  (-> request
      (auth-owner)
      ;; cgi/or behavior
      ;; assertion fail => set cgi/*condition* => break threading, escape => show condition
      ;; assertion succeed => continue threading => [`e ("owner")]
      (cgi/assert #_or (constantly cgi/*condition*))
      (and [`e (-> request :path :unmatched)])))

(defn auth-me [request]
  (if (some-> request :path :unmatched (->> (some #{"me"})))
    :authorized
    (cgi/set-condition! :unauthorized)))

(defmethod cgi/assert-failed ::auth-owner [assertion] [::auth-owner cgi/*condition*])
(defmethod cgi/assert-failed `auth-me [assertion] [`auth-me cgi/*condition*])

;; assertion fail owner => set cgi/*condition* => assert-failed method for ::auth-owner
;; assertion fail me => set cgi/*condition* => assert-failed method for `auth-me
;; assertions succeed => [`f ("me" "owner")]
(defn ^:cgi f [request]
  (cgi/assert ::auth-owner (auth-owner request))
  (cgi/assert (auth-me request))
  [`f (-> request :path :unmatched)])

;; catch all handler
(defn ^:cgi cgi [request]
  [`cgi (-> request :path :unmatched)])
