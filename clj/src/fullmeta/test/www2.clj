(ns fullmeta.test.www2
  (:require [fullmeta.cgi :as cgi]))

(alias 'www2 'fullmeta.test.www2)

;; catch all handler
(defn ^:cgi cgi [request]
  [`cgi (-> request :path :unmatched)])
