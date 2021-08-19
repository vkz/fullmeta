(ns fullmeta.cgi
  (:refer-clojure :exclude [assert])
  (:require [fullmeta.prelude             :as prelude :refer [make expect]]
            [clojure.tools.namespace.file :as ns.file]
            [clojure.tools.namespace.find :as ns.find]
            [clojure.string               :as string]
            [clojure.pprint               :as pp]
            [clojure.java.io              :as io]
            [clojure.test                 :as t]
            [ring.util.response           :as ring.response]
            [ring.util.request            :as ring.request]
            [ring.util.codec              :as ring.codec]))
(alias 'core 'clojure.core)
(alias 'cgi 'fullmeta.cgi)


(def ^:dynamic *reload-routes*
  "Recompute routes on every request. Set to nil in production where
  routes do not change."
  true)


(when t/*load-tests*
  (defn- test-server [handler] (fn [request] (handler request)))
  (defn- default-test-handler [request] :default-test-handler)
  ;; instead of cgi/my-default-response return the response unchanged
  (defn- as-is [request response] response))


(defprotocol protocol
  (middleware [context])
  (route [request] [context request])
  (handle [request] [context request])
  (reload [context]))


(def ^:dynamic *side* nil)
(defrecord my-request [record])
(defrecord their-request [record])
(defn my
  ([request]
   (or (:my request) (my-request. request)))
  ([request method & args]
   (binding [cgi/*side* :my]
     (apply method (my request) args))))
(defn their
  ([request]
   (or (:their request) (their-request. request)))
  ([request method & args]
   (binding [cgi/*side* :their]
     (apply method (their request) args))))


(declare chop-ext safe-path match-domain drop-blanks)


;;* cgi/root-record -------------------------------------------------- *;;
(defrecord root-record [path dir script])
(def root root-record)
(defmethod make cgi/root [_ path] (make cgi/root :from path))
(defmethod make [cgi/root :from String] [_ _ path]
  (core/assert (io/resource path))
  (let [root (-> path io/resource io/as-file)
        script (-> root .getCanonicalPath (str ".clj") io/as-file)]
    (core/assert (and (.exists root) (.isDirectory root)))
    (root-record. path root (when (.exists script) script))))
(defmethod make [cgi/root :from cgi/root] [_ _ root] root)
(defmethod make [cgi/root :from java.net.URL] [_ _ path] (throw "implement me?"))
(defmethod make [cgi/root :from java.io.File] [_ _ path] (throw "implement me?"))


;;* cgi/script-record ------------------------------------------------ *;;
(defrecord script-record [file ns methods])
(def script script-record)
;; TODO do we want to e.g. compute cgi-root based path to store with script? We'd need cgi context
;; and ensure that script is in fact rooted there
(defmethod make cgi/script [_ obj] (make cgi/script :from obj))
(defmethod make [cgi/script :from cgi/script] [_ _ script] script)
(defmethod make [cgi/script :from String] [_ _ resource]
  (core/assert (io/resource resource))
  (make cgi/script :from (-> resource io/resource io/as-file)))
(defmethod make [cgi/script :from java.io.File] [_ _ file]
  (core/assert (.exists file) "expected cgi/script file to exist")
  (let [ns (some-> file (ns.file/read-file-ns-decl (:read-opts ns.find/clj)) second symbol)]
    ;; HACK to ensure find-ns succeeds, ideally we want to use clojure.tools.namespace to build
    ;; dependency graph from cgi root including root script, then execute load-file in that order
    ;; avoiding the refresh logic. This "solution" may produce puzzling results. Does load-file even
    ;; work correctly if common dependency hasn't been loaded yet? I suspect (:require ..) in file's
    ;; ns form takes care of that?
    (when-not (-> (loaded-libs) (contains? ns))
      (load-file (.getCanonicalPath file)))
    (core/assert (find-ns ns) (str "unable to infer namespace for cgi/script " file))
    ;; :methods nil but in prod where methods won't change we may want to pre-compute and store them
    (script-record. file (find-ns ns) nil)))


;;* cgi/context-record ----------------------------------------------- *;;
(defrecord context-record [root routes my their fail])
(def context context-record)
(defmethod make cgi/context [_ obj] (make cgi/context :from obj))
(declare relative-to my-default-response cgi-routes)
(defmethod make [cgi/context :from clojure.lang.IPersistentMap] [_ _ cgi-settings]
  (core/assert (contains? cgi-settings :root))
  (let [root (make cgi/root :from (cgi-settings :root))]
    (map->context-record
     {:root root
      :routes (cgi-routes root)
      :my {:request (or (-> cgi-settings :my :request) (-> cgi-settings :my) (constantly nil))
           :response (or (-> cgi-settings :my :response) my-default-response)}
      :their {:request (or (-> cgi-settings :their :request) (-> cgi-settings :their) (constantly nil))}
      :fail #(or (not (match-domain (or (cgi-settings :domain) #".*") %))
                 (some-> cgi-settings :fail (.invoke %)))})))
(defmethod make [cgi/context :from String]      [_ _ path] (make cgi/context :from {:root path}))
(defmethod make [cgi/context :from cgi/root]    [_ _ root] (make cgi/context :from {:root root}))
(defmethod make [cgi/context :from cgi/context] [_ _ context] context)


(defn relative-to [script root]
  (when-let [path (some-> script :file .getCanonicalPath chop-ext)]
    (when-let [root (some-> root :dir .getCanonicalPath)]
      (when (-> path (string/starts-with? root))
        (some-> (string/replace-first path root "")
                (safe-path)
                (vector script))))))


(defn cgi-routes [{:keys [dir script] :as root}]
  (->> (ns.find/find-sources-in-dir dir ns.find/clj)
       (prelude/cons-if #(some-> % io/as-file (.exists)) script)
       (map #(-> (make cgi/script %) (relative-to root)))
       (reduce prelude/conj-if {})))


(t/set-test cgi-routes
  (defn- test-routes [root] (-> (make cgi/context root) :routes keys set))
  (expect (-> "fullmeta/test/www1" test-routes (get "/"))  "should have root script")
  (expect (-> "fullmeta/test" test-routes (get "/")) = nil "should have no root script")
  (expect (-> "fullmeta/test/www1" test-routes) clojure.set/superset? #{"/" "/bar/bam" "/bar/baz"})
  (expect (-> "fullmeta/test" test-routes)      clojure.set/superset? #{"/www1/bar/baz" "/www1/bar/bam" "/www1" "/www2"}))


;; TODO render helpful info like e.g. what we could do with request, what response needs to be etc
(defn my-default-response [request response]
  {:status 200
   :headers {"Content-Type" "text/plain; charset=utf-8"}
   :body (with-out-str
           (println "Response:")
           (println "---------")
           (pp/pprint response)
           (newline))})


;;* cgi/request-record ----------------------------------------------- *;;
(defrecord request-record [context original my their path])
(def request request-record)
(defmethod make cgi/request [_ obj & args] (apply make cgi/request :from obj args))
(defmethod make [cgi/request :from cgi/request]
  ([_ _ request] request)
  ([_ _ request context] (assoc request :context (make cgi/context context))))
(defmethod make [cgi/request :from clojure.lang.IPersistentMap]
  ([_ _ obj]
   (core/assert (and (contains? obj :context)
                (or (contains? obj :original)
                    (contains? obj :request))))
   (make cgi/request
     (or (:original obj)
         (:request obj))
     (:context obj)))
  ([_ _ request context]
   (core/assert (ring.request/path-info request) "Expected a ring-compatible request")
   (let [context (make cgi/context :from context)
         request (request-record. context request nil nil {:original (ring.request/path-info request)})]
     (assoc request
            :my ((-> context :my :request) request)
            :their ((-> context :their :request) request)))))
(defmethod make [cgi/request :from String] [_ _ www-path context]
  ;; for testing?
  (make cgi/request {:uri www-path} context))


;;* cgi/handler-record ----------------------------------------------- *;;
(declare assert-failed *condition*)
(defrecord handler-record [script method var path next]
  clojure.lang.IFn
  (invoke [handler request]
    (or
     (binding [cgi/*condition* nil]
       (try
         (some-> handler (get :var)
                 ;; update request with :unmatched path chunks
                 (.invoke (update request :path merge (:path handler))))
         (catch clojure.lang.ExceptionInfo e
           ;; TODO log assertion failure somewhere
           (if (-> e ex-data :type (= ::cgi/assertion))
             ;; once we have our custom CgiAssertionError we won't need to reattach the error
             (assert-failed (assoc (ex-data e) :error e))
             (throw e)))))
     ;; try the next handler when present
     (when-let [next-handler (:next handler)]
       (next-handler request)))))
(def handler handler-record)
(defmethod make cgi/handler [_ obj & args] (apply make cgi/handler :from obj args))
(defmethod make [cgi/handler :from clojure.lang.IPersistentMap] [_ _ {:keys [script method path next]}]
  (when-let [method (some-> method symbol)]
    (let [{:keys [ns methods]} script]
      (when-let [var (or (get methods method) (ns-resolve ns method))]
        (when (or (some-> var meta :cgi)
                  (some-> var meta :protocol meta :cgi))
          (handler-record. script method var path next))))))
(declare request-handlers cgi-route)
(defmethod make [cgi/handler :from cgi/request] [_ _ request]
  (when-not (-> request :context :fail (.invoke request))
    (when-let [handlers (request-handlers request)]
      (reduce
       (fn [handler next-handler]
         (assoc handler :next next-handler))
       (first handlers)
       (next handlers)))))


(defn request-handlers [request]
  (let [[matched [method & args :as unmatched] script] (cgi-route request)]
    (when script
      ;; NOTE we establish the order in which handlers are tried here! When we start matching routes
      ;; from path front we'll often have more than just these 2 handlers - every path parent.
      (->> [(make cgi/handler :from {:script script :method method :path {:matched matched :unmatched args}})
            (make cgi/handler :from {:script script :method 'cgi :path {:matched matched :unmatched unmatched}})]
           (filter some?)
           (seq)))))


;; TODO memoize route lookup for production? Since path maybe anything we may want to limit it to a
;; reasonable depth (number of path elements) and / or limit size of our cache with some reasonable
;; caching policy e.g. drop oldest or smth
(defn cgi-route [request]
  (let [cgi-routes (-> request :context :routes)]
    (when-let [path (-> request :original ring.request/path-info safe-path)]
      ;; TODO we're chopping off from the back to arrive at the longest route, but we could chop
      ;; from the front instead and every match would potentially constitute a handler: root, its
      ;; subdir, its subdir etc. Now we could reverse the order and have a chain of "fallback"
      ;; handlers with the root being the last resort. This simplifies the routing algo and IMO
      ;; makes for better semantics: if script at path fails to handle the request, we drop to its
      ;; parent script until root. If latter fails, we delegate to potentially "non-cgi" handlers.
      (loop [path path
             suffix nil]
        (if-let [script (get cgi-routes path)]
          [path (drop-blanks suffix) script]
          ;; chop path elements one by one from the end until we find a
          ;; route that matches. This ensures that we find the longest
          ;; matching route and return the unmatched suffix
          (let [[m path' suffix'] (re-matches #"(.*)/([^/]*)$" path)]
            (if m
              (recur path' (conj suffix suffix'))
              ;; TODO default to the root script? Not a fan of looking up naked "/" in the routes
              ;; table. Aren't we better of extracting this from cgi/root record somehow?
              [nil (drop-blanks (conj suffix path)) (get cgi-routes "/")])))))))
;; => [matched-path unmatched-path-suffix-sequence script]


(t/set-test cgi-route
  (expect (->> "fullmeta/test/www1" (make cgi/request "..")            (cgi-route))          = nil                    "should fail to route unsafe path 1")
  (expect (->> "fullmeta/test/www1" (make cgi/request "../www1")       (cgi-route))          = nil                    "should fail to route unsafe path 2")
  (expect (->> "fullmeta/test/www1" (make cgi/request "./..")          (cgi-route))          = nil                    "should fail to route unsafe path 3")
  (expect (->> "fullmeta/test/www1" (make cgi/request ".")             (cgi-route) (take 2)) = '("/" nil)             "should handle . 1")
  (expect (->> "fullmeta/test/www1" (make cgi/request "./")            (cgi-route) (take 2)) = '("/" nil)             "should handle . 2")
  (expect (->> "fullmeta/test/www1" (make cgi/request "")              (cgi-route) (take 2)) = '("/" nil)             "should not need front /")
  (expect (->> "fullmeta/test/www1" (make cgi/request "/bar")          (cgi-route) (take 2)) = '(nil ("bar"))         "should accumulate unmatched suffix 1")
  (expect (->> "fullmeta/test/www1" (make cgi/request "/bar/booz")     (cgi-route) (take 2)) = '(nil ("bar" "booz"))  "should accumulate unmatched suffix 2")
  (expect (->> "fullmeta/test/www1" (make cgi/request "/bar/baz")      (cgi-route) (take 2)) = '("/bar/baz" nil)      "should match script")
  (expect (->> "fullmeta/test/www1" (make cgi/request "bar/baz/bash")  (cgi-route) (take 2)) = '("/bar/baz" ("bash")) "should match script and keep unmatched suffix 1")
  (expect (-> (make cgi/request "bar" "fullmeta/test/www1") cgi-route (get-in [2 :ns]) str)  = "fullmeta.test.www1"   "should delegate no match to root script when exists")
  (expect (-> (make cgi/request "bar" "fullmeta/test")      cgi-route (get 2))               = nil                    "should not delegate no match when root script does not exist"))


;;* Extend protocols ------------------------------------------------- *;;
(declare response? not-found?)
(extend-protocol protocol
  String
  (middleware [cgi-root]         (-> (make cgi/context :from cgi-root) (middleware)))
  (route      [cgi-root request] (-> (make cgi/context :from cgi-root) (route request)))
  (handle     [cgi-root request] (-> (make cgi/context :from cgi-root) (handle request)))
  (reload     [cgi-root]         (-> (make cgi/context :from cgi-root)))

  context-record
  (middleware [context]
    ;; close over context assuming it won't change; in dev we reload on every request below
    (let [context (make cgi/context :from context)]
      (fn [handler]
        (fn
          ([request]
           ;; reload context in dev; in prod *reload-routes* ought to have been set to nil
           (let [context (if cgi/*reload-routes* (reload context) context)
                 cgi-request (make cgi/request request context)
                 response (handle cgi-request)]
             (cond
               ;; nil or false mean the current handler either has no idea how to deal with the
               ;; request or one of its required conditions not satisfied. Maybe useful to log cases
               ;; like this.
               (not response)        #_=> (handler request)
               (not-found? response) #_=> (handler request)
               (response? response)  #_=> response
               ;; for anything else either user must supply custom handler or we'll run
               ;; my-default-response handler. This ensures that we are likely to get a response
               ;; whenever route matched, so that even when user doesn't know or remember how to
               ;; create a valid response, they still get feedback - "least surprise" approach.
               :else ((-> (:my context) :response) cgi-request response))))
          ([request respond raise]
           (core/assert false "TODO implement async cgi middleware"))))))
  (route [context request]
    (->> (make cgi/request request context)
         (make cgi/handler)))
  (handle [context request]
    (->> (make cgi/context context)
         (make cgi/request request)
         (handle)))
  (reload [context]
    (assoc context :routes (-> context :root cgi-routes)))

  clojure.lang.IPersistentMap
  (middleware [context]         (-> (make cgi/context :from context) (middleware)))
  (route      [context request] (-> (make cgi/context :from context) (route request)))
  (handle     [context request] (-> (make cgi/context :from context) (handle request)))
  (reload     [context]         (-> (make cgi/context :from context)))

  request-record
  (route [request]
    (make cgi/handler request))
  (handle [request]
    (let [handler (or (route request) (constantly nil))]
      (handler request)))
  (reload [request]
    ;; not useful imo but for completness?
    (make cgi/request request (-> request :context))))


;;* Condition system ------------------------------------------------- *;;


(declare assert-expand)

(def ^:dynamic *condition* nil)
(defn set-condition! [value] (set! *condition* value) nil)

(defmacro assert
  ([form] (assert-expand {:key nil :form form :assert-failed nil}))
  ([a b] (if (keyword? a)
           (assert-expand {:key a :form b :assert-failed nil})
           (assert-expand {:key nil :form a :assert-failed b}))))

;; TODO ought to be a function of two arguments: assertion map and request, else the only way for us
;; to pass the request is via (assert condition-form else-fn) where else-fn closes over request at
;; call site. Without access to request'our condition system is limited to error-reporting and not
;; suitable for redirects that may requiref original form-params and path. We may also want to be
;; able to pass arguments to assert-failed handler from assert site?
(defmulti assert-failed
  (fn [assertion]
    (or (-> assertion :key)
        (-> assertion :form first))))

(def ^:dynamic *assert-failed-default*
  (fn assert-failed-default [{:keys [form key] :as assertion}]
    ;; TODO appropriate default behavior or should we log and delegate to next handler?
    (throw
     (-> assertion :error))))

(defmethod assert-failed :default [{:keys [assert-failed] :as assertion}]
  ((or assert-failed *assert-failed-default*) assertion))

(defn- assert-expand [{:keys [key form assert-failed]}]
  ;; TODO this feels so wrong like I'm conflating two phases compile and runtime. Would this always
  ;; work? I'm not even confident I want to dispatch on the form's first symbol
  (let [sym (when-let [sym (cond
                             (and (seq? form) (-> form first symbol?)) (first form)
                             (symbol? form) form)]
              ;; or is this what's &env for? I suck at Clojure macros
              (some-> (ns-resolve *ns* sym) (symbol)))]
    `(let [form# '~form]
       (or ~form
           (throw
            (ex-info
             (str "cgi/assert " (when ~key (str "for key " ~key)) " failed: " (pr-str form#))
             {:type ::cgi/assertion
              :key (or ~key '~sym)
              :form form#
              :assert-failed ~assert-failed}))))))


(when t/*load-tests*
  (defn- www1
    ([] (test-server
         (-> default-test-handler
             ((middleware {:root "fullmeta/test/www1" :my {:response as-is}})))))
    ([request] ((www1) request))))


(t/deftest test-condition-system
  (expect (www1 {:uri "/"})           = '[fullmeta.test.www1/cgi nil])
  (expect (www1 {:uri "/foo/bar"})    = '[fullmeta.test.www1/cgi ("foo" "bar")])
  (expect (www1 {:uri "/a"}) thrown? clojure.lang.ExceptionInfo)
  (expect (www1 {:uri "/b/bar"})      = '[42 ("bar")]                                   "should prefer local assertion handler")
  (expect (www1 {:uri "/c"})          = '[fullmeta.test.www1/cgi ("c")]                 "should delegate to next handler")
  (expect (www1 {:uri "/d"})          = :unauthorized                                   "should set cgi/*condition*")
  (expect (www1 {:uri "/d/owner"})    = '[fullmeta.test.www1/d ("owner")]               "should pass cgi/assert")
  (expect (www1 {:uri "/e"})          = :unauthorized                                   "should fail cgi/assert in -> pipeline")
  (expect (www1 {:uri "/e/owner"})    = '[fullmeta.test.www1/e ("owner")]               "should pass cgi/assert in -> pipeline")
  (expect (www1 {:uri "/f"})          = '[:fullmeta.test.www1/auth-owner :unauthorized] "should fail first assertion and use its assert-failed method")
  (expect (www1 {:uri "/f/owner"})    = '[fullmeta.test.www1/auth-me :unauthorized]     "should fail second assertion and use its assert-failed method")
  (expect (www1 {:uri "/f/me/owner"}) = '[fullmeta.test.www1/f ("me" "owner")]          "should pass two assertions"))


;;* Reverse route ---------------------------------------------------- *;;
(def ^:dynamic *root* nil)
(def ^:dynamic *routes* nil)

(defn -route [handler handler-sym extra-path params]
  (when-let [{:keys [file name] :as handler-meta}
             ;; TODO this is broken because turns out :file in meta of a var as produced by Clojure
             ;; compiler is relative to classpath root that is (-> file io/resource io/as-file
             ;; .getCanonicalPath) will give an valid and existing absolute path. However when you
             ;; do cider-load-buffer :file on meta becomes absolute path. In the first case you want
             ;; to chop cgi/*root* (i.e. relative path) but in the latter you want to chop the
             ;; absolute cananical path. My impl below assumed and did the latter. Hacky fix is to
             ;; check for absolute and dispatch on that.
             (meta
              (cond
                (var? handler) handler
                (symbol? handler-sym) (clojure.core/resolve handler-sym)
                :else (throw
                       (java.lang.IllegalArgumentException.
                        (format "cgi/route expected handler symbol or var, given %s" handler-sym)))))]
    (core/assert (:cgi handler-meta) "expected a cgi handler")
    (when (and file name)
      (let [cgi-root (-> *root* io/resource io/as-file .getCanonicalPath)
            routes (or *routes* (cgi-routes *root*))
            path (-> file (chop-ext) (string/replace-first cgi-root "") (safe-path))]
        (when (-> routes (contains? path))
          ;; TODO not quite what we want? We want actual path i.e. one of the keys in cgi-routes
          ;; table. Do we have to (cgi-route url) now? Mapping of path <-> handler won't change in
          ;; production and should be known (collected) before we even launch.
          (let [url (str
                     (io/as-file
                      ;; TODO only reason we do shit like this (str (io/as-file ..)) is to normalize
                      ;; path produced e.g. remove extra / say //cgi => /cgi etc. Proper solution is
                      ;; to manipulate path datatype a-la Racket rather than strings damn it.
                      (str path "/" name (some-> extra-path safe-path))))
                params (some->> params
                                (map (fn [[k v]] (str (clojure.core/name k) "=" (ring.codec/url-encode v))))
                                (interpose "&")
                                (apply str "?"))]
            (str url params)))))))

#_
(defmacro route
  ([handler] `(route ~handler nil nil))
  ([handler arg] `(let [arg# ~arg
                        path# (when (string? arg#) arg#)
                        params# (when (map? arg#) arg#)]
                    (route ~handler path# params#)))
  ([handler path params]
   ;; NOTE We deliberately eval handler just to have the compiler check and signal if it is unbound
   `(-route ~handler '~handler ~path ~params)))

(comment
  (route handler "" params)
  (.exists (io/as-file (io/resource "pledge/www")))
  (binding [cgi/*root* "pledge/www"]
    (cgi/route chop-ext))
  ;; comment
  )

;;* Helpers ---------------------------------------------------------- *;;


(defn chop-ext [path]
  (clojure.string/replace path #"\.[^./]*$" ""))


(defn safe-path
  "Return canonical path always rooted in / or nil when unsafe"
  [path]
  (let [path (ring.codec/url-decode path)
        dir-traversals? (-> path (string/split #"/") (set) (contains? ".."))
        allowed-chars? (re-matches #"^[a-zA-Z0-9_/?.-]*$" path)
        prefix (-> "." io/as-file (.getCanonicalPath))]
    (when (and allowed-chars? (not dir-traversals?))
      (let [path (-> (str "./" path)
                     (io/as-file)
                     (.getCanonicalPath)
                     (string/replace-first prefix ""))]
        (case path
          "" "/"
          path)))))


(t/set-test safe-path
  (expect (safe-path "")                     = "/")
  (expect (safe-path "/")                    = "/")
  (expect (safe-path "foo/bar?/baz_BAM////") = "/foo/bar?/baz_BAM")
  (expect (safe-path "foo/./bame")           = "/foo/bame")
  (expect (safe-path "foo/./ba.me")          = "/foo/ba.me")
  (expect (not (safe-path "foo/~/bame")))
  (expect (not (safe-path "foo/../bame"))))


(defn match-domain [pattern request]
  (-> request (my :server-name) (or "")
      (->> (re-matches (re-pattern pattern)))))


(defn- drop-blanks [sequence]
  (seq (remove string/blank? sequence)))


(def ^:private response? ring.response/response?)
(defn- not-found? [response]
  (and (response? response)
       (-> response :status (= 404))))


;;* Tests ------------------------------------------------------------ *;;


(when t/*load-tests*
  (defn- www
    ([] (test-server
         (-> default-test-handler
             ;; try www1 then www2 in that order
             ((middleware {:root "fullmeta/test/www2" :domain "www2" :my {:response as-is}}))
             ((middleware {:root "fullmeta/test/www1" :domain #"www1" :my {:response as-is} :fail #(-> % :path :original (= "/foo"))})))))
    ([request] ((www) request))))


(t/deftest test-multiple-cgi-roots
  (expect (www {:uri "/"})                        = :default-test-handler         "should fail :domain test when server-name is missing")
  (expect (www {:server-name "www" :uri "/"})     = :default-test-handler         "should fail :domain test when server-name fails to match")
  (expect (www {:server-name "www1" :uri "/"})    = '[fullmeta.test.www1/cgi nil] "should pass :domain test")
  (expect (www {:server-name "www1" :uri "/foo"}) = :default-test-handler         "should :fail fast")
  (expect (www {:server-name "www2" :uri "/"})    = '[fullmeta.test.www2/cgi nil] "should dispatch to the next cgi root"))


(comment
  (let [req (make cgi/request {:uri "pledge/create/something"} "pledge/www")]
    ((make cgi/handler req) req))
  ;; comment
  )
