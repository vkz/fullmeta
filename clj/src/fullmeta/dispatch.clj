(ns fullmeta.dispatch
  (:refer-clojure :exclude [isa? parents ancestors descendants derive underive global-hierarchy])
  (:require [fullmeta.prelude :as prelude :refer [make]]))


(alias 'dispatch 'fullmeta.dispatch)


;; TODO Both Clojure multimethods with their :default value and our dispatch with hierarchy bottom
;; value have an interesting yet non-obvious failure mode due to the caching Clojure MultiFn impl
;; does whenever a method is looked up. It uses a simple Clojure map to cache dispatch value to
;; effective method. In the presence of :default (or in our case bottom) method this cache may grow
;; linear to the size of potential dispatch values. Difficult to estimate how often this can occur
;; in practice. IMO it is mostly limited to cases where you rely heavily on :defaut (bottom) catch
;; all method. Still, this can happen. This suggests that our poly (multi) method impl should take
;; cache implementation as a parameter so we may e.g. leverage clojure.core.cache lib.


(def default-hierarchy-bottom ::dispatch/default)

(defrecord hierarchy-record [parents descendants ancestors bottom])
(def hierarchy hierarchy-record)
(defmethod make dispatch/hierarchy
  ([_] (hierarchy-record. {} {} {} default-hierarchy-bottom))
  ([_ h-or-b]
   (cond
     (associative? h-or-b) (make dispatch/hierarchy h-or-b (or (:bottom h-or-b) default-hierarchy-bottom))
     (keyword? h-or-b) (hierarchy-record. {} {} {} h-or-b)
     :else (throw
            (IllegalArgumentException. "expected a map, hierarchy record or bottom keyword"))))
  ([_ h bottom]
   (assert (keyword? bottom) "Hierarchy bottom value must be a keyword")
   (hierarchy-record.
    (or (:parents h) {})
    (or (:descendants h) {})
    (or (:ancestors h) {})
    bottom)))

(def global-hierarchy (hierarchy-record. {} {} {} ::dispatch/default))

(comment
  ;; TODO I am not a fan of this whole defaulting to global-hierarchy business. IMO not specifying
  ;; hierarchy saves you little and I'd much rather be explicit about it and demand its use as a
  ;; reminder we dispatch off of a hierachy. We could then switch to protocol:
  (defprotocol hierarchy-protocol
    (bottom      [h])
    (parents     [h tag])
    (ancestors   [h tag])
    (descendants [h tag])
    (isa?        [h tag parent])
    (derive      [h tag parent])
    (underive    [h tag parent]))
  ;; comment
  )

(defn bottom
  ([] (bottom global-hierarchy))
  ([hierarchy] (or (get hierarchy :bottom) default-hierarchy-bottom)))


;; NOTE these are mostly copy-paste of clojure.core with minor tweaks for our hierarchy impl. Sucks
;; that we are forced to do that.

;; alias ^:private core defs
(def into1 #'clojure.core/into1)
(def reduce1 #'clojure.core/reduce1)
(comment
  ;; too evil?
  (alter-meta! (def into1 (var-get #'clojure.core/into1)) merge (meta #'clojure.core/into1))
  (alter-meta! (def reduce1 (var-get #'clojure.core/reduce1)) merge (meta #'clojure.core/reduce1))
  ;; comment
  )

(defn isa? "see `clojure.core/isa?`"
  ([child parent] (isa? global-hierarchy child parent))
  ([h child parent]
   (or (= child parent)
       ;; NOTE(fullmeta) line below is the only change from clojure.core. Essetially it means that
       ;; any value isa? bottom. Note that even [a b] isa? bottom now. It follows then that when
       ;; supplied multimethod for bottom always matches but is dominated by any other matching
       ;; value. This also renders multimethod :default method value useless whenever bottom method
       ;; is defined. Not obvious that indeed e.g. [x bottom] should isa? bottom. Think about it.
       (= parent (bottom h))
       (and (class? parent) (class? child)
            (. ^Class parent isAssignableFrom child))
       (contains? ((:ancestors h) child) parent)
       (and (class? child) (some #(contains? ((:ancestors h) %) parent) (supers child)))
       (and (vector? parent) (vector? child)
            (= (count parent) (count child))
            (loop [ret true i 0]
              (if (or (not ret) (= i (count parent)))
                ret
                (recur (isa? h (child i) (parent i)) (inc i))))))))

(defn parents "see `clojure.core/parents`"
  ([tag] (parents global-hierarchy tag))
  ([h tag] (not-empty
            (let [tp (get (:parents h) tag)]
              (if (class? tag)
                (into1 (set (bases tag)) tp)
                tp)))))

(defn ancestors "see `clojure.core/ancestors`"
  ([tag] (ancestors global-hierarchy tag))
  ([h tag] (not-empty
            (let [ta (get (:ancestors h) tag)]
              (if (class? tag)
                (let [superclasses (set (supers tag))]
                  (reduce1 into1 superclasses
                           (cons ta
                                 (map #(get (:ancestors h) %) superclasses))))
                ta)))))

(defn descendants "see `clojure.core/descendants`"
  ([tag] (descendants global-hierarchy tag))
  ([h tag] (if (class? tag)
             (throw (java.lang.UnsupportedOperationException. "Can't get descendants of classes"))
             (not-empty (get (:descendants h) tag)))))

(defn derive
  "see `clojure.core/derive`"
  ([tag parent]
   (assert (namespace parent))
   (assert (or (class? tag) (and (instance? clojure.lang.Named tag) (namespace tag))))

   (alter-var-root #'global-hierarchy derive tag parent) nil)
  ([h tag parent]
   (assert (not= tag parent))
   (assert (not= tag (bottom h)))
   (assert (or (class? tag) (instance? clojure.lang.Named tag)))
   (assert (instance? clojure.lang.Named parent))

   (let [tp (:parents h)
         td (:descendants h)
         ta (:ancestors h)
         tf (fn [m source sources target targets]
              (reduce1 (fn [ret k]
                         (assoc ret k
                                (reduce1 conj (get targets k #{}) (cons target (targets target)))))
                       m (cons source (sources source))))]
     (or
      (when-not (contains? (tp tag) parent)
        (when (contains? (ta tag) parent)
          (throw (Exception. (print-str tag "already has" parent "as ancestor"))))
        (when (contains? (ta parent) tag)
          (throw (Exception. (print-str "Cyclic derivation:" parent "has" tag "as ancestor"))))
        (make dispatch/hierarchy
          {:parents (assoc (:parents h) tag (conj (get tp tag #{}) parent))
           :ancestors (tf (:ancestors h) tag td parent ta)
           :descendants (tf (:descendants h) parent ta tag td)}
          (bottom h)))
      h))))

(defn underive "see `clojure.core/underive`"
  ([tag parent] (alter-var-root #'global-hierarchy underive tag parent) nil)
  ([h tag parent]
   (assert (not= tag (bottom h)))
   (assert (not= parent (bottom h)) "attempt to underive tag from hierarchy bottom value")
   (let [parentMap (:parents h)
         childsParents (if (parentMap tag)
                         (disj (parentMap tag) parent) #{})
         newParents (if (not-empty childsParents)
                      (assoc parentMap tag childsParents)
                      (dissoc parentMap tag))
         deriv-seq (flatten (map #(cons (key %) (interpose (key %) (val %)))
                                 (seq newParents)))]
     (if (contains? (parentMap tag) parent)
       (reduce1 #(apply derive %1 %2) (make dispatch/hierarchy (bottom h))
                (partition 2 deriv-seq))
       h))))

(comment
  (derive ::foo ::bar)
  (derive ::bar ::baz)
  (derive String ::string)
  global-hierarchy
  (isa? String ::string)
  (isa? String ::default)
  (isa? ::foo ::default)
  (isa? ::foo ::bar)
  (isa? ::bar ::baz)
  (isa? ::foo ::baz)

  (isa? [::foo ::bar] [::bar ::baz])
  (isa? [::foo ::bar] [::default ::baz])
  (isa? [::foo ::bar] ::default)
  (isa? [::foo ::bar] [::default ::default ::default])

  (underive ::bar ::baz)
  (isa? ::bar ::baz)
  (isa? ::foo ::baz)
  ;; comment
  )
