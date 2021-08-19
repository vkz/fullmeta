(ns make
  (:require [prelude           :as dev.prelude]
            [clojure.java.io   :as io]
            [clojure.edn       :as edn]
            [clojure.string    :as string]
            [clojure.pprint    :as pp]
            [cljs.build.api    :as build]
            [cljs.repl.browser :as browser]
            [cljs.repl         :as cljs.repl]
            [nrepl.server      :as nrepl.server]
            [cider.piggieback  :as piggie]))


(comment
  ;; TODO How do I programmatically compile form to JS?
  (require '[cljs.compiler.api :as cljs.comp])
  (require '[cljs.analyzer.api :as cljs.ana])
  (cljs.comp/emit
   (cljs.ana/empty-state)
   (cljs.ana/analyze (cljs.ana/empty-env) '(fn [_] (swap! count inc))))
  ;; comment
  )


(def ^:dynamic called-from-command-line? nil)

(defn command-line-args->task [& args]
  (let [[task & args] args]
    (if task
      (if (keyword? task)
        task
        (-> task edn/read-string keyword))
      :default)))

(defmulti make command-line-args->task)

(defmethod make :default [& args]
  (println "[make] nothing to do")
  (when called-from-command-line?
    (System/exit 1)))

;; NOTE should \"refresh\" ever break or excibit weird behavior
;; comment this entire pre-compilation step and delete everything
;; inside \"classes\" dir! This whole thing is highly experimental and
;; Google tells me pre-compilation and tools.ns/refresh do not play
;; nice. I attempt to speed up REPL startup time as per this:
;;
;; https://clojure.org/guides/dev_startup_time
;;
;; In theory this shouldn't prevent us from repl-driven development
;; because Clojure compiler checks timestamps on sources vs .class
;; files it finds in "classes" dir and recompiles as needed.
;;
;; https://github.com/clojure/tools.namespace#warnings-and-potential-problems
;; says tools.namespace refresh does not work with .class files
;; around. Also won't work with ring-devel, which I'm not using atm.

;; TODO better way to do it is to have separate REPL namespace. This
;; would run precompile and we could also disable-reload! there
;; completely! We'd also start nrepl there and wrap in middleware as
;; needed instead of having it in deps.edn

(defmethod make :clj/compile
  [& args]
  (let [ns (or
            (some-> args second symbol find-ns .name)
            'user)
        cp (or
            (some-> (dev.prelude/deps-edn) :custom :compile-path)
            *compile-path*)]
    (let [cp (io/as-file cp)]
      (assert
       (and (.exists cp) (.isDirectory cp))
       (format "compile-path %s does not exist" cp)))
    (printf "[make] compiling namespace %s with its transitive
  dependencies. This will put generated .class files to %s. Adding
  this folder to classpath, should improve JVM startup time.\n"
            ns *compile-path*)
    (println "[make] see: https://clojure.org/guides/dev_startup_time\n")
    (newline)
    (println "[make] !!! AOT compiled .class files prevent \"refresh\" !!!\n")
    (binding [*compile-path* cp
              *compile-files* true]
      ;; trick to transitively compile all dependencies. (compile ns)
      ;; would only compile ns without its deps.
      ((var clojure.core/load-all) ns true true))
    (when-not called-from-command-line?
      (println "[make] re-compiled clj code")
      (println "[make] you may want to refresh your namespaces"))))

;; We don't want tools.ns/refresh (and therefore cider-ns-refresh) to
;; trigger another :reload-all in the above compile-all. This would
;; drop the user ns in cider and prevent you from typing anything at
;; the repl (althogh eval in buffer would still work just fine). To
;; avoid this we prevent refresh from unloading user (it'll still be
;; reloaded). This way the defonce below will actually run once, so
;; compilation will only ever happen the first time we connect and not
;; every time we refresh.
#_
(tools.ns/disable-unload!)
;; This simply adds :clojure.tools.namespace.repl/unload false to the
;; namespace metadata
#_
(defonce precompiled
  (compile-all))
;; Compile our project once now that defonce is honored.


(def cljs-build-opts
  {:output-dir "resources/public/js"
   :output-to "resources/public/js/main.js"
   ;; asset-path is not a filesystem path! Rather it is
   ;; supposed to be path that our webserver can actually
   ;; find. Generated main.js above will use it as prefix
   ;; for all scripts it requires including goog.base.js
   :asset-path "js"
   ;; without this iframe that connects to cljs repl is
   ;; not injected or rather (i think) there is no call
   ;; to connect to the repl. That would be the one we
   ;; start below with make :cljs/repl. This particular
   ;; preload uses whatever HOST and PORT default values
   ;; are in cljure.browser.repl. PORT just happens to
   ;; match our repl 9000 below. But we could swap this
   ;; preload with our own namespace that would simply
   ;; call clojure.browser.repl/connect with our own
   ;; settings. It could also do other setup e.g. see:
   ;;
   ;; https://clojurescript.org/reference/compiler-options#preloads
   ;;
   ;; TODO
   :preloads '[clojure.browser.repl.preload
               cljs.user
               ;; fullmeta.dev
               ]
   :source-map true
   ;; :output-dir ""
   ;; :main 'main
   :verbose true
   :optimizations :none})


;; TODO This does about what I expect. It actually compiles the file
;; and moves it where needed. But it also overwrites js/cljs_deps.js
;; as I thought it would and renders all of my other namespaces
;; unavailable. IMO I want partially mimic what build/build is doing
;; but then use lower level API to build the ns dependency graph and
;; generate cljs_deps.js myself
(defn compile-user []
  (let [opts {:output-dir "resources/public/js"
              :asset-path "js"
              :source-map true
              :main 'main
              :verbose true
              :optimizations :none}]
    (build/build "clj/dev/user.cljs" opts)))
#_
(println (compile-user))


;; what happens if build here fails? If this prevents repl from
;; being started, we better wrap it in try, report but still
;; start the repl, so the error maybe fixed.
(defmethod make :cljs/build [& args]
  (println "building clj/src")
  (build/build "clj/src" cljs-build-opts))
#_(make :cljs/build)


(defmethod make :cljs/watch [& args]
  (let [cljs-watch-opts {:watch-fn (fn [] (println "CLJS re-build success!!!"))
                         :watch-error-fn (fn [] (println "CLJS re-build fail!!!"))}]
    (build/watch "clj/src" (merge
                            cljs-build-opts
                            cljs-watch-opts))))


;; NOTE Something subtle yet important and often overshadowed by
;; "magical" tools like shadow, cider etc. CSJL repl is nothing but a
;; call to clojure function (assuming we're not using stand-alone CLJS
;; compiler but rather the Clojure implementation). So "starting cljs
;; repl" amounts to opening another connection (repl buffer) to the
;; nRepl server and running Clojure code. There. We've just regained
;; agency with our tools. Fuck magic.
(defmethod make :cljs/repl [& args]
  (make :cljs/build)
  (println "starting REPL")
  (let [env (browser/repl-env
             :launch-browser false
             ;; :launch-browser true
             :port 9000
             ;; :working-dir "resources/public/js"
             :src "clj/src"
             ;; :static-dir ["resources/public/js"
             ;;              "resources/public"
             ;;              "resources/public/assets"]
             )
        repl (if called-from-command-line?
               cljs.repl/repl
               ;; piggie repl may only be run within nRepl
               ;; environment, i.e. when Cider nRepl client connects
               ;; to nRepl server it can start piggie repl. See
               ;; https://github.com/nrepl/piggieback#clojure-cli-aka-toolsdeps
               piggie/cljs-repl)]
    (apply repl env (mapcat identity cljs-build-opts))))

(comment
  (require 'cljs.util)
  (cljs.util/ns->source 'user))

#_
(make :cljs/build)

#_
(make :cljs/repl)


(defmethod make :java/compile [task & args]
  (println (apply str "compiling .java files in " (or (seq args) ["clj/src"])))
  (doseq [dir (or (seq args) ["clj/src"])]
    (dev.prelude/javac dir)))

#_
(make :java/compile)


(defn -main [& args]
  (binding [called-from-command-line? true]
    (apply make *command-line-args*)))
