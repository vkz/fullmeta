(ns fullmeta.css
  (:require [fullmeta.prelude :as prelude :refer [make expect]]
            [clojure.string :as string]
            [clojure.test   :as t]))


(alias 'css 'fullmeta.css)


;; All HTML tags as specified in https://developer.mozilla.org/en-US/docs/Web/HTML/Element
(def html-tag? #{"html" "base" "head" "link" "meta" "style" "title" "body" "address" "article" "aside" "footer" "header" "h1" "h2" "h3" "h4" "h5" "h6" "hgroup" "main" "nav" "section" "blockquote" "dd" "div" "dl" "dt" "figcaption" "figure" "hr" "li" "ol" "p" "pre" "ul" "a" "abbr" "b" "bdi" "bdo" "br" "cite" "code" "data" "dfn" "em" "i" "kbd" "mark" "q" "rb" "rp" "rt" "rtc" "ruby" "s" "samp" "small" "span" "strong" "sub" "sup" "time" "u" "var" "wbr" "area" "audio" "img" "map" "track" "video" "embed" "iframe" "object" "param" "picture" "source" "canvas" "noscript" "script" "del" "ins" "caption" "col" "colgroup" "table" "tbody" "td" "tfoot" "th" "thead" "tr" "button" "datalist" "fieldset" "form" "input" "label" "legend" "meter" "optgroup" "option" "output" "progress" "select" "textarea" "details" "dialog" "menu" "summary" "slot" "template"})
;; TODO for CSS purposes we may want to drop meta tags :head :link :meta :style :title?


(defn escape-css-selector [s]
  (string/escape
   (str (symbol s))
   {\. "\\." \/ "\\/"}))


;; TODO optionally cut CGI root?
(defn parse-css-selector [k]
  (cond-> {:raw k}
    (keyword? k)
    (merge
     (let [ns (namespace k)
           [name id] (->> (string/split (name k) #"[#]")
                          (map (partial prelude/nilify string/blank?)))
           tag? (html-tag? name)]
       {:tag (when tag?
               {:type :tag
                :raw (keyword name)
                :string name})
        :id (when id
              {:type :id
               :raw (keyword (symbol ns (str "#" id)))
               :string (str (symbol ns id))})
        :class (when name
                 (when (or ns (not tag?))
                   {:type :class
                    :raw (keyword (symbol ns name))
                    :string (str (symbol ns name))}))}))))


(defprotocol protocol
  (classes [o] [o selector] [o selector self?])
  (render  [o])
  (combine [o1 o2]))


(defprotocol selector-protocol
  (parse [o] [o selector-type])
  (write [s]))


(defrecord selector-record [type raw string split])
(def selector selector-record)


(extend-protocol css/selector-protocol

  selector-record
  ;; ------------
  (parse
    ([selector] selector)
    ([selector selector-type]
     (if (= selector-type (get selector :type))
       selector
       (some-> (get-in selector [:split selector-type])
               (map->selector-record)
               (assoc :split (get selector :split))))))
  (write [selector]
    (case (get selector :type)
      :class (str "." (escape-css-selector (get selector :string)))
      :id (str "#" (escape-css-selector (get selector :string)))
      :tag (get selector :string)
      (throw (ex-info
              (format "Don't know how to write %s selector" (get selector :type))
              selector))))

  clojure.lang.Keyword
  ;; -----------------
  (parse
    ([k]
     (let [{:keys [tag id class] :as split} (parse-css-selector k)]
       (-> (or id class tag
               ;; in order of CSS priority
               (throw
                (java.lang.IllegalArgumentException.
                 (format "Keyword %s failed to produce a selector" k))))
           (assoc :split split)
           (map->selector-record))))
    ([k selector-type]
     (let [split (parse-css-selector k)]
       (some-> split
               (get selector-type)
               (assoc :split split)
               (map->selector-record)))))
  (write [k] (write (parse k)))

  java.lang.String
  ;; -------------
  (parse
    ([s] (selector-record. :raw s s nil))
    ([s selector-type]
     (when (= :class selector-type)
       (selector-record. :class s s nil))))
  (write [s] s)

  ;; TODO
  clojure.lang.Symbol
  ;; ----------------
  (parse
    ([s] (selector-record. :class s (str s) nil))
    ([s selector-type]
     (when (= :class selector-type)
       (selector-record. :class s (str s) nil))))
  (write [s] (str "." (escape-css-selector (str s)))))


(defmethod make css/selector
  ([_ o selector-type]
   (let [supported #{:tag :class :id}]
     (assert
      (supported selector-type)
      (format "Expected one of %s, but received %s" supported selector-type))
     (assert
      (satisfies? css/selector-protocol o)
      (format "expected object that satisfies selector-protocol"))
     (parse o selector-type)))
  ([_ o]
   (assert
    (satisfies? css/selector-protocol o)
    (format "expected object that satisfies selector-protocol"))
   (parse o)))


(defprotocol string-protocol (^String -str [x]))
(extend-protocol css/string-protocol
  clojure.lang.Keyword (-str [k] (name k))
  clojure.lang.Ratio   (-str [r] (str (float r)))
  java.lang.String     (-str [s] s)
  java.lang.Object     (-str [o] (str o))
  nil                  (-str [_] ""))


(def unitless-prop?
  (into #{} (for [key ["animation-iteration-count" "box-flex" "box-flex-group" "box-ordinal-group" "column-count" "flex" "flex-grow" "flex-positive" "flex-shrink" "flex-negative" "flex-order" "grid-row" "grid-column" "font-weight" "line-clamp" "line-height" "opacity" "order" "orphans" "tab-size" "widows" "z-index" "zoom" "fill-opacity" "stop-opacity" "stroke-dashoffset" "stroke-opacity" "stroke-width"]
                  prefix ["" "-webkit-" "-ms-" "-moz-" "-o-"]]
              (str prefix key))))


(defn normalize-key [k]
  (-> (-str k)
      (string/replace #"[A-Z]" (fn [ch] (str "-" (string/lower-case ch))))
      (string/replace #"^ms-" "-ms-")))


(defn normalize-value [key value]
  (let [key (-str key)]
    (cond
      (unitless-prop? key) (string/trim (-str value))
      (number? value) (str value (when (not= 0 value) "px"))
      :else (string/trim (-str value)))))


(defrecord prop-record [key val]
  css/protocol
  (render [prop]
    (let [key (normalize-key (get prop :key))
          val (normalize-value key (get prop :val))]
      (str key ":" val ";"))))
(def prop prop-record)
(defmethod make css/prop [_ key val]
  ;; TODO validate by only allowing known CSS properties?
  (prop-record. key val))


(t/deftest test-css-prop-rendering
  (t/is (= "line-height:24;"     (render (make css/prop :line-height 24)))       "should not add \"px\" to unitless prop")
  (t/is (= "-webkit-box-flex:3;" (render (make css/prop :-webkit-box-flex 3)))   "should handle prefixed unitless prop")
  (t/is (= "margin-top:3px;"     (render (make css/prop :margin-top 3)))         "should add \"px\"")
  (t/is (= "margin-top:0;"       (render (make css/prop :margin-top 0)))         "should not add \"px\" to 0 value")
  (t/is (= "padding-bottom:1em;" (render (make css/prop :padding-bottom "1em"))) "should leave units alone when present")
  (t/testing "should handle React style props"
    (t/is (= "font-weight:10;" (render (make css/prop :fontWeight 10))))
    (t/is (= "-ms-flex:1;"     (render (make css/prop "msFlex" 1))))
    (t/is (= "-ms-flex:1;"     (render (make css/prop :ms-flex 1))))))


(defrecord rule-record [selector props]
  css/protocol
  (render [rule]
    (if-let [props (some->> (get rule :props)
                            (prelude/nilify empty?)
                            (map render)
                            (string/join))]
      (str (render selector) "{" props "}")
      "")))
(def rule rule-record)
(defmethod make css/rule [_ s props]
  (rule-record.
   (make css/selector s)
   (some->> (dissoc props :classes)
            (prelude/nilify empty?)
            (map #(apply make css/prop %)))))


(defn all-classes [css sel conj?]
  (when-let [{:keys [type raw]} (make css/selector sel)]
    (let [direct-classes
          (when raw
            (get-in css [:classes raw]))

          recursive-classes
          (when direct-classes
            (mapcat #(all-classes css % :conj) direct-classes))]
      (if (or conj? #_(= :class type))
        (conj (vec recursive-classes) raw)
        (vec recursive-classes)))))


(defrecord css-record [rules classes])
(def css css-record)
(defmethod make css/css

  ([_ rules]
   (assert
    (and (map? rules)
         (not (empty? rules)))
    "Expected a non-empty map of CSS rules")
   (if (instance? css/css rules) rules
       (apply make css/css (mapcat identity rules))))

  ([_  selector props & rules]
   (let [rules (->> rules (partition 2) (cons [selector props]))]
     (css-record.
      (map #(apply make css/rule %) rules)
      (some->> rules
               (map (fn [[s props]] [s (get props :classes)]))
               (filter second)
               (mapcat identity)
               (prelude/nilify empty?)
               (apply array-map))))))


(def ^{:dynamic true} *css* (css-record. nil nil))


(extend-protocol protocol

  css-record
  ;; -------
  (classes
    ([css]
     (get css :classes))
    ([css sel]
     (when-let [selector (make css/selector sel)]
       (classes css selector (= :class (:type selector)))))
    ([css sel self?]
     (prelude/nilify
      empty?
      (all-classes css sel self?))))
  (render [css]
    (when-let [rules (get css :rules)]
      (string/join (map render rules))))

  selector-record
  ;; ------------
  (classes
    ([sel] (classes *css* sel))
    ([sel self?] (classes *css* sel self?)))
  (render [sel]
    (case (get sel :type)
      :class (str "." (escape-css-selector (get sel :string)))
      :id (str "#" (escape-css-selector (get sel :string)))
      (str
       (or (get sel :string)
           (get sel :raw)))))

  clojure.lang.Keyword
  ;; -----------------
  (classes
    ([k] (classes *css* (parse k)))
    ([k self?] (classes *css* (parse k) self?)))
  #_(render [k] (render (parse k)))

  java.lang.String
  ;; -------------
  (classes [css & _] nil)
  (render [css] css))


(when t/*load-tests*
  (defn test-css []
    (make css/css
      {"form li + li" {:margin-top "1rem"}
       ::#amount      {:classes [:foo] :border "1px solid red"}
       :body          {:width 400 :classes ['body] :margin-top 20 :margin-left 60}
       ::on           {:classes '[inline-block text-gray-700 text-right pl-1 pr-1]}
       ::input        {:classes '[::on border text-gray-800]}
       ::textarea     {:classes [::input 'w-full]}})))


(t/deftest test-class-expansion
  (expect (classes (test-css) ::#amount)   = [:foo]  "should handle id selector")
  (expect (classes (test-css) "bla")       = nil     "should not expand string selector")
  (expect (classes (test-css) "bla" :self) = ["bla"] "should treat self as class when forced")
  (expect (classes (test-css) :body)       = '[body] "should expand tag selector")
  (expect (classes (test-css) ::textarea)  = '[inline-block text-gray-700 text-right pl-1 pr-1 ::on border text-gray-800 ::input w-full ::textarea] "should expand classes recursively and inject self")
  (expect (classes (test-css) ::on false)  = '[inline-block text-gray-700 text-right pl-1 pr-1] "should not inject self when forced not to")
  (expect (classes (test-css) 'on)         = '[on]   "should treat symbol as class selector"))


(t/deftest test-css-rendering
  (expect (render (test-css)) = "form li + li{margin-top:1rem;}#fullmeta\\.css\\/amount{border:1px solid red;}body{width:400px;margin-top:20px;margin-left:60px;}"))
