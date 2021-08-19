(ns fullmeta.methods
  (:refer-clojure :exclude [isa? defmulti defmethod])
  (:require [fullmeta.prelude :as prelude :refer [make]]
            [fullmeta.dispatch :as dispatch :refer [isa? global-hierarchy]])
  (:import fullmeta.MultiFn))


(alias 'methods 'fullmeta.methods)

(def check-valid-options #'clojure.core/check-valid-options)


(defmacro defmulti
  "see `clojure.core/defmulti`"
  {:arglists '([name docstring? attr-map? dispatch-fn & options])}
  [mm-name & options]
  (let [docstring   (if (string? (first options))
                      (first options)
                      nil)
        options     (if (string? (first options))
                      (next options)
                      options)
        m           (if (map? (first options))
                      (first options)
                      {})
        options     (if (map? (first options))
                      (next options)
                      options)
        dispatch-fn (first options)
        options     (next options)
        m           (if docstring
                      (assoc m :doc docstring)
                      m)
        m           (if (meta mm-name)
                      (conj (meta mm-name) m)
                      m)
        mm-name (with-meta mm-name m)]
    (when (= (count options) 1)
      (throw (Exception. "The syntax for defmulti has changed. Example: (defmulti name dispatch-fn :default dispatch-value)")))
    (let [options   (apply hash-map options)
          default   (get options :default :default)
          hierarchy (get options :hierarchy #'global-hierarchy)]
      ;; NOTE Our hierarchies with bottom value render :default matching value semantics bizarre and
      ;; perhaps not intuitive. Better avoid supplying the :default option altogether instead choose
      ;; to define method for the bottom value or vectors with bottom values in them. Note that
      ;; whenever dispatch value isa? method value, the latter is also always isa? hierarchy bottom
      ;; value and therefore dominates it, so more specific value always wins. If both :default and
      ;; bottom methods are defined, bottom dominates simply because any dispatch value isa? bottom.
      ;; With no bottom method supplied "normal" :default value semantics will delegate to its
      ;; matching method if supplied. I think :default and bottom shouldn't coexist and bottom ought
      ;; to replace :default method value entirely in our own multimethod implementation.
      (check-valid-options options :default :hierarchy)
      `(let [v# (def ~mm-name)]
         (when-not (and (.hasRoot v#) (instance? fullmeta.MultiFn (deref v#)))
           (def ~mm-name
             (new fullmeta.MultiFn ~(name mm-name) ~dispatch-fn ~default ~hierarchy)))))))

(defmacro defmethod
  "see `clojure.core/defmethod`"
  [multifn dispatch-val & fn-tail]
  `(. ~(with-meta multifn {:tag 'fullmeta.MultiFn}) addMethod ~dispatch-val (fn ~@fn-tail)))


;;* Play
(comment
  (defmulti my-method (fn [x y] [x y]))

  (defmethod my-method [:x :y]
    [_ _]
    :x-y)

  ;; NOTE using the default bottom value here!
  (defmethod my-method [::dispatch/default :y]
    [_ _]
    :default-y)

  ;; NOTE but we rely on the default "catch all" method value here which is :default. In general I
  ;; think I ought to stick with hierarchy bottom logic and avoid relying on :default value.
  (defmethod my-method :default
    [_ _]
    :default)

  ;; override the above :default catch all with dispatch to bottom which always matches because any
  ;; dispatch value isa? bottom
  #_
  (defmethod my-method ::dispatch/default
    [_ _]
    ::dispatch/default)

  (my-method :x :y)
  (my-method nil :y)
  (my-method :x nil)
  ;; comment
  )
