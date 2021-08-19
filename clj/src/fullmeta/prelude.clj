(ns fullmeta.prelude
  (:refer-clojure :exclude [read-string declare])
  (:require [clojure.pprint :as pp]
            [clojure.string :as string]
            [clojure.test :as t]
            [clojure.edn :as edn]))

(defrecord unbound [id]
  clojure.lang.IFn
  (invoke [u] (throw (ex-info "Declared but unbound identifier" u)))
  (invoke [u _] (.invoke u))
  (invoke [u _ _] (.invoke u))
  (invoke [u _ _ _] (.invoke u))
  (invoke [u _ _ _ _] (.invoke u))
  (invoke [u _ _ _ _ _] (.invoke u))
  (invoke [u _ _ _ _ _ _] (.invoke u))
  (invoke [u _ _ _ _ _ _ _] (.invoke u))
  (invoke [u _ _ _ _ _ _ _ _] (.invoke u))
  ;; ought to be enough? How does Clojure core get around to handle any num of args?
  )

(defmacro declare [& names]
  ;; TODO is this really the best way to fully qualify passed in identifier?
  (let [vals (map #(unbound. `~(symbol (str *ns*) (name %))) names)]
    `(do ~@(map #(list 'def (vary-meta %1 assoc :declared true) %2) names vals))))


;; TODO Multimethod dispatch that recursively dispatches for [] collection dispatch values and
;; therefore allows :default sub values inside. At first sight IIUC all it might take is redefining
;; clojure.core/isa? so that it recurses into [] but otherwise stays the same. I should be able to
;; follow my emacs-lisp isa? implementation. Sadly with-redefs won't cut it cause it'll break every
;; library that ever dispatched on []. It is one line change in clojure.lang.MultiFn though:
;;
;;   https://github.com/clojure/clojure/blob/master/src/jvm/clojure/lang/MultiFn.java#L31
;;
;; have it point to my isa?


;; TODO How stupid is this really?! Records "dynamically" generate respective classes. E.g.
;; (defrecord selector ...) does not actually produce selector var that's available outside of the
;; namespace. Instead locally selector expands into some "class" special thing. Consequently when
;; you e.g. (require '[pledge.css :as css]) elsewhere css/selector isn't available cause defrecord
;; doesn't actually create such var. Only two ways to refer to records require and then import
;; (import '[pledge.css selector]) but that obviously produce naked type names or use fully
;; qualified name without importing it (require is enough): pledge.css.selector. I hate this!
;; Macroexpand defrecord once to see what's going on.
;;
;; For now I'm using a convention to circumvent this e.g.:
;; (ns pledge.foons)
;; (alias 'foons 'pledge.foons)
;; (defrecord foo-record [])
;; (def foo foo-record)
;; (defmethod make foons/foo [_ ...])
;;
;; This still creates a problem in e.g. (extend-protocol ...) cause macro expects symbolic class
;; names as delimiters for types that extend said protocol that is foons/foo won't evaled to produce
;; a class. Stupid thing all to avoid extra parens, sigh. Although maybe it effect AOT somehow?
;;
;; We could also be clever and macro the above convention.
;;   (our/defrecord foo [])
;; or
;;   (def foo (record foo []))
;; or
;;   (def foo (record foo-record []))
;; or even anonymous, but this would prevent us from using (extend-protocol) with such records
;;   (def foo (record []))
;;
;; Almost feel like I should have my own defrecord, extend-protocol macros but this may break
;; tooling or some e.g. AOT compilation expectations.
;;
;; or crazy
;;
;; (record css/selector)
;; => expand
;; (let [sym 'css/selector
;;       ns-name (namespace sym)
;;       type-name (name sym)
;;       ns (get (ns-aliases *ns*) (symbol ns-name))]
;;   (assert ns "no such alias")
;;   (resolve (symbol (str ns "." type-name))))
;;
;; *ns* there maybe a problem cause its dynamic. Alternative we might define our own defrecord that
;; e.g. creates a selector-record type (class) and binds selector var to that type. We can go
;; further and bind selector var to our own "class" record that implements IFn whose only purpose in
;; life will be to invoke our (make ...) method correctly. And we'd have our make handle our "class"
;; like records correctly.
;;
;; Another possible solution to that problem is custom (prelude/import [pledge.html :as html]) that
;; collects all records defined in pledge.html and somehow creates bindings for classes to vars. Not
;; even sure if its possible to abuse Clojure namespaces like that. Even then we must be careful
;; cause we are adding vars that weren't defined by their authors - bad idea.


;; TODO Is there a way to ensure that clojure.tools.logging is only
;; ever loaded through this function i.e. backend is chosen? Maybe
;; have a global state we set, check if loaded then do nothing, else
;; unload clojure.tools.logging inside (locking (Object.) ...) and
;; require it again?
(defn enable-logging!
  "Must be called before 'clojure.tools.logging is ever loaded"
  []
  ;; we must ensure tools.logging picks correct backend before it is
  ;; ever loaded
  (System/setProperty
   "clojure.tools.logging.factory"
   "clojure.tools.logging.impl/slf4j-factory")
  ;; now we load tools logging for the first time
  (require 'clojure.tools.logging))


;;* Testing

;; NOTE in production all we have to do to "disable" test evaluation - their code becomes nop and
;; names won't even make it into namespaces - is setting test/*load-tests* to false. We can probably
;; do this in System or via deps.edn somehow

;; NOTE Only did this cause whatever lib Cider uses to produce test reporting expects "expected"
;; value to appear as the first argument e.g. (= expected actual) ditto (= expected expr).
;; clojure.test doesn't attempt to do this magic instead if quotes the entire form passed to t/is
;; and prints that as expected, then prints the same form with all args evaled as actual. I'm not
;; convinced "better" reporting is worth extra dependency Cider brings in and therefore complexity.
(defmacro expect
  ([expr] `(t/is ~expr))
  ([expr msg] `(t/is ~expr ~msg))
  ([expr op expected] `(t/is (~op ~expected ~expr)))
  ;; NOTE => works perfectly well with thrown? but atm not with thrown-with-msg?:
  ;; (expect expr thrown? AssertionError) will work
  ([expr op expected msg] `(t/is (~op ~expected ~expr) ~msg)))

;; TODO Racket-style collapse test definitions: set-test, deftest, deftest-

;; TODO Inline per fn tests with Clojure t/set-test or t/with-test. See docs. Would tools support
;; that? Cider seems to pick em up.
(comment
  (defn foo [] (let [] 42))
  (t/set-test foo
    (t/is (= (foo) 42))
    (t/is (< (foo) 0)))
  ;; comment
  )

;; TODO Is this idea based on t/set-test worth persuing?
(comment
  (require '[clojure.test :as test])
  (defmulti defun-keyword (fn [kw kw-body & _] kw))
  (defmethod defun-keyword :test [_ test-body [fname & _]]
    ;; TODO this ought to work as middleware to be applicable in general in that it potentially alters
    ;; (defn foo ...) and returns that and maybe other code that must run at top-level. Then next
    ;; defun-keyword receives current code for defn and top-level bodies and alters former and
    ;; potentially adds its own body to the latter and so on. Else it only works for code that doesn't
    ;; touch function definition itself.
    `(alter-meta! (var ~fname) assoc :test (fn [] ~@test-body)))

  ;; deserves a better name probably
  (defmacro defun [fname & all]
    (let [all (cons fname all)
          f (take-while (complement keyword?) all)
          e? (complement keyword?)
          bodies (loop [body (drop-while e? all)
                        bodies nil]
                   (if (seq body)
                     (recur
                      (drop-while e? (rest body))
                      (conj bodies (defun-keyword (first body) (take-while e? (rest body)) f)))
                     bodies))]
      `(do
         (defn ~@f)
         ~@bodies)))

  (comment
    (defun foo []
      (let []
        42)

      :test
      (test/is (= (foo) 42))
      ;; failing test
      (test/is (< (foo) 0)))

    (foo)
    (:test (meta (var foo)))
    (test/run-tests *ns*)

    ;; or without the macro at all
    (-> (defn foo []
          (let [] 42))

        (test/with-test
          (test/is (= (foo) 42))
          (test/is (< (foo) 0))))

    ;; or but this introduces a new name
    (defn foo [] (let [] 42))
    (test/deftest foo-test
      (test/is (= (foo) 42))
      (test/is (< (foo) 0)))

    ;; this is better cause it simply alters foo metadata, no new name required
    (defn foo [] (let [] 42))
    (test/set-test
        foo
        (test/is (= (foo) 42))
        (test/is (< (foo) 0)))
    ;; comment
    ))

;;* Makers


#_
(defmulti make (fn [type & _args] type))

;; TODO should we use its own hierarchy and avoid polluting the global one?
(defmulti make
  (fn
    ([what] what)
    ([what arg] what)
    ;; NOTE :from obj pattern lets users add more ways to "make" an object usually :from their own
    ;; record type essentially making "make" user extensible
    ([what from? object & _args]
     (if (= from? :from)
       ;; TODO one problem with this is that dispatch cache may grow indefinitely because we map
       ;; potentially unbound number of objects into several dispatch values corresponding to object
       ;; types via (type object). Exactly why we should take over caching, allow user to specify it
       ;; and maybe even limit cache size by default.
       ;;
       ;; On second thought, not a problem most of the time. Clojure only caches dispatch values -
       ;; result of applying the dispatch function to args. Unbound gropth possible but rare.
       [what
        ;; TODO do we even want to special case keywords and symbols? Maybe (type object) is enough?
        ;; If all we ever do is rely on hierarchies then keywords & symbols can still appear in
        ;; dispatch values e.g. [person :from ::map]
        :from
        (if (and (or (keyword? object)
                     (symbol? object))
                 (namespace object))
          ;; TODO IMO given the derive semantics treating only fully qualified symbols and
          ;; keywords specially is reasonable, but it may trip me up down the road. Think.
          object
          (type object))]
       what))))

(comment
  ;; TODO Usability: should dispatch values be [to-type :from from-type] rather than [to-type from-type]?
  (defmethod make [employee :from person] [_ _ v] impl)
  (defmethod make [employee :from clojure.lang.IPersistentMap] [_ _ v] impl)
  ;; or our current suffice?
  (defmethod make [employee #_from person] [_ _ v] impl)
  (defmethod make [employee #_from clojure.lang.IPersistentMap] [_ _ v] impl)

  ;; TODO Be nice to always have the equivalent of map->Foo constructor. Either we adopt a convention
  ;; or we provide a default implementation that somehow does that.

  ;; Here is a good convention that relies on our current :from (type object) implementation
  (defrecord person-record [name surname])
  (def person person-record)
  (defmethod make [person #_from clojure.lang.PersistentArrayMap] [_ _ map]
    (map->person-record map))

  ;; With fullmeta.methods and multi default dispatch we could even provide a default implementation
  (defmethod make [:fullmeta.dispatch/default clojure.lang.PersistentArrayMap] [type _ map]
    ;; this feels extremely hacky but you can't deny that it is dynamic. We rely on every every
    ;; class having the form of ns.classname. We should probably also check for classes without ns.
    (if-let [[_ ns nm] (re-matches #"(.+)\.([^.]+)" (print-str type))]
      (if-let [ctor (resolve (symbol ns (str "map->" nm)))]
        (ctor map)
        (throw (format "Don't know how to make %s :from PersistentArrayMap" type)))
      (throw (format "Don't know how to make %s :from PersistentArrayMap" type))))

  ;; Even more generic impl is to match whatever interface constitutes a map or rather we should
  ;; look at what map->Foo actually needs to do its job.
  (defmethod make [:fullmeta.dispatch/default clojure.lang.IPersistentMap] [] body)
  ;; because
  (t/is (and
         (isa? clojure.lang.PersistentArrayMap clojure.lang.IPersistentMap)
         (isa? clojure.lang.PersistentHashMap clojure.lang.IPersistentMap)
         ;; even more generic
         (isa? clojure.lang.IPersistentMap clojure.lang.ILookup)))

  ;; comment
  )


;; TODO Does make need a :default like this? If combination of types fails to match a method, report
;; that - least surprise and informs that we must supply method. I'll have to experiment to see what
;; kind of errors this :default produces and if its not confusing. Actually (new class ...) produces
;; reasonable error - No matching ctor found, so maybe its ok?
(defmethod make :default [class & args]
  (assert (class? class) (format "Default make can only handle classes but given [%s] of type [%s]" class (type class)))
  (eval `(new ~class ~@args)))
;; NOTE something to be aware of is that Clojure multifn is dumb when arrity error happens, it'll
;; signal e.g. pledge.css/eval34003/fn--34004 received wrong num of args which isn't any help.
;; Luckily unless executed at top-level or when entire buffer is loaded stack trace has location.

(comment

  (defrecord foo-record [val])
  (def foo foo-record)
  (defmethod make foo
    ([_] (foo-record. nil))
    ([_ v] (map->foo-record (if (map? v) v {:val v}))))

  (defmethod make [foo #_from :kw]
    [_ _from _ v]
    (make foo v))

  (defrecord bar-record [val])
  (def bar bar-record)
  (defmethod make [foo #_from bar]
    ([_ _from bar] (make foo (:val bar))))

  (make foo 42)
  (make foo)
  (make foo :from (make bar 42))
  (make foo :from :kw 42)
  ;; comment
  )


;;* Conformers


;; this is to accommodate case where you want to proxy field lookup to e.g. a map or another record
;; stored in the field of the record being conformed. Works out ok in this case, but I wonder:
;;
;; TODO How to implement lookup semantics that tries record fields first, then tries one of its
;; field's values. You can't simply take over ILookup, cause then there's no way to get field values
;; of the record itself. If we only cared about (get foo key) then we could take over Map interface,
;; implement its (get ..) method in terms of ILookup's (valAt ..), but then all other methods of
;; looking up key would continue to use normal Clojure semantics, and it is annoying to extend other
;; Map methods. Alex Miller said you'll have to use (deftype ..) and roll out your own "record" like
;; type if you want to change ILookup semantics - no way around it. That means implementing all
;; interfaces that records expect. That's really not worth it.
(defprotocol conform-protocol
  (field-get [record field]))
(extend-protocol conform-protocol
  clojure.lang.IRecord
  (field-get [record field] (get record field)))

(defmulti conform
  (fn
    ([record]
     (assert (record? record))
     (type record))
    ([record field]
     (assert (record? record))
     [(type record) field (type (field-get record field))])))


(defmethod conform clojure.lang.IRecord [record]
  (reduce conform record (-> record keys)))


;; This one is redundant with the above asserts, right?
(defmethod conform Object [object]
  (throw
   (java.lang.IllegalArgumentException.
    (format "Expected a Clojure record got %s" (type object)))))

;; NOTE What should we do when [(type record) key (-> record key type)] fails to match, i.e. user
;; failed to supply matching methods. We have two options:
;;   (1) we throw and notify the user early, it forces user to supply methods for every key;
;;   (2) we assume field is expected to remain unchanged and simply return the record intact,
;;   puts less pressure to cover every key but could it lead to hard to detect data errors?
;;
;; Another way of thinking about (1) vs (2) is that in (2) every method we define signifies a
;; special case of a value at key e.g. normally it is a number, but on rare occasion that it
;; turns out to be a String attempt to read a number to replace it, put differently we assume
;; that most of the time the record is already conformant.
;;
;; Ultimately for now I think (2) is more natural so by providing :default method below we
;; choose (2). Without that :default method we would've essentially had (1) i.e. simply report
;; method missing. I'll have to test this in the wild to see which I prefer. Maybe provide both
;; under different name e.g. conform vs conform-strict (or conform! or conforms)?
(comment
  ;; Whether we choose (1) or (2) an interesting "catch all keys and their value types" trick:
  (doseq [key (-> (make proto/pledge) keys)] (derive key ::pledge-key))
  (defmethod conform [proto/pledge ::pledge-key Object]
    ;; would happily match any proto/pledge key of any type! With semantics (1) user may supply that
    ;; and effectively implement semantics (2); with default semantics (2) user may supply this
    ;; method that throws an error and therefore effectively implement (1).
    ))

(defmethod conform :default
  ([record] (throw "This method should never ever fire. It is a bug!"))
  ([record field] record))

(comment
  ;; TODO alternatively we could also use our version of dispatch with multi :default. Maybe I
  ;; should also implement and provide this under yet different name? Remember however that our
  ;; dispatch relies on our own version of the global hierarchy (which luckily works as expected for
  ;; classes and built in types) but still we must be careful to extend our hierarchy with our derive.
  ;;
  ;; Should fire first
  (defmethod conform [proto/pledge :r (dispatch/bottom)] [pledge _] (update pledge :r #(make proto/identity %)))
  ;; should fire second (not obviously the right choice! The two dispatch value (above vs below) are
  ;; ambiguous - neither isa? the other unless we choose to always "prefer" left to right - then
  ;; above isa? below (because :r strictly isa? ::default) assuming we short circuit at that point
  ;; but that isn't obviously correct semantics.
  (defmethod conform [proto/pledge (dispatch/bottom) String])
  ;; should hopefully fire third (or last)
  (defmethod conform [proto/pledge (dispatch/bottom) (dispatch/bottom)])
  ;; comment
  )

(comment
  (defrecord foo-conf [a b])
  (defmethod conform [foo-conf :a String] [foo _] (update foo :a edn/read-string))
  (defmethod conform [foo-conf :a nil] [foo _] (update foo :a (constantly nil)))
  (defmethod conform [foo-conf :b nil] [foo _] (update foo :b (constantly 42)))

  (expect (conform (map->foo-conf {})) = (foo-conf. nil 42))
  (expect (conform (map->foo-conf {:a "42"})) = (foo-conf. 42 42))
  ;; comment
  )

;;* Helpers


(defn nilify
  ([pred] (partial nilify pred))
  ([pred s]
   (when-not (pred s) s)))


;; TODO clojure.core/conj has arbitrary arity - takes a sequence of elements to conj onto coll, so
;; this works: (conj {} ['a 1] ['b 2]). We could allow this by always requiring pred? (2nd case).
;; Such semantics would make the following work:
#_(->> {:a 1 :b 2 :c nil} (apply (conj-if second) {}) (= {:a 1 :b 2}))
;; although atm this works nicely
#_(->> {:a 1 :b 2 :c nil} (reduce (conj-if second) {}) (= {:a 1 :b 2}))
;; so maybe not worf it?

;; TODO consider conj-if that preserves value produced by the predicate. Would this be useful?

(defn conj-if
  ([pred?] (partial conj-if pred?))
  ([coll el] (if (some? el) (conj coll el) coll))
  ([pred? coll el] (if (pred? el) (conj coll el) coll)))
(t/set-test conj-if
  (expect (conj-if even? [2] 4) = [2 4])
  (expect (conj-if even? [2] 3) = [2])
  (expect (conj-if [] 42) = [42])
  (expect (conj-if [] nil) = [])
  (expect (-> [] ((conj-if even?) 42)) = [42])
  (expect (reduce (conj-if even?) [] [1 2 3 4]) = [2 4])
  (expect (->> {:a 1 :b 2 :c nil} (reduce (conj-if second) {})) = {:a 1 :b 2}))


(defn cons-if
  ([pred?] (partial cons-if pred?))
  ([el coll] (if (some? el) (cons el coll) coll))
  ([pred? el coll] (if (pred? el) (cons el coll) coll)))
(t/set-test cons-if
  (expect (cons-if even? 4 [2]) = '(4 2))
  (expect (cons-if even? 3 [2]) = '(2))
  (expect (cons-if 42 []) = '(42))
  (expect (cons-if nil []) = [])
  (expect (->> [] ((cons-if even?) 42)) = '(42))
  (expect (reduce #(cons-if even? %2 %1) [] [1 2 3 4]) = '(4 2)))
