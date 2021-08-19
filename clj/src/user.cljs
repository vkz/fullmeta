(ns cljs.user)

;; NOTE OMFG cljs repl does expect to find the stupid user on the
;; classpath and will use whatever namespace it itself defines.
;; Fucking kill me now! How about following appropriate classpath
;; conventions people???

(println "hello from user.cljs")
(js/console.log "hello from user.cljs")

(defn browser-reload
  ([]
   (js/location.reload))
  ([after & args]
   ;; TODO reload will wipe out all js, so how do we reliably ensure
   ;; that an after function is invoked after reload? Silly (static)
   ;; way of doing it would be to simply declare such fn in cljs.user.
   ;; Is that sufficient or do we want to be dynamic? Then we may want
   ;; to keep around the symbol for fn on the clojure side somehow and
   ;; trigger its eval after reload somehow. That's why we may want to
   ;; have bidirectional communication btw frontend and backend. Then
   ;; say <body onload=f> could communicate to the server that its
   ;; loaded and at that point we could issue an eval. That is we need
   ;; a listener on the server side that could do that.
   (js/location.reload)))
