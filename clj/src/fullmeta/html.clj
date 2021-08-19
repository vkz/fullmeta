(ns fullmeta.html
  (:refer-clojure :exclude [keyword])
  (:require
   [fullmeta.prelude :as prelude :refer [make]]
   [fullmeta.css :as css]
   [clojure.string :as string]
   [clojure.pprint :as pprint]
   [clojure.test :as t]
   ;; for debug
   [rum.server-render :refer [render-static-markup]])
  (:import
   [clojure.lang PersistentVector Atom IPersistentVector ISeq IFn Named Numbers Ratio Keyword Symbol PersistentArrayMap]))


(alias 'html 'fullmeta.html)


;;* Stream protocol --------------------------------------------------- *;;


;; TODO is it really faster than doing the natural Clojure thing or does my -push! indirection prevent that?
(defn append!
  ([^StringBuilder sb] sb)
  ([^StringBuilder sb s0]                     (doto sb (.append s0)))
  ([^StringBuilder sb s0 s1]                  (doto sb (.append s0) (.append s1)))
  ([^StringBuilder sb s0 s1 s2]               (doto sb (.append s0) (.append s1) (.append s2)))
  ([^StringBuilder sb s0 s1 s2 s3]            (doto sb (.append s0) (.append s1) (.append s2) (.append s3)))
  ([^StringBuilder sb s0 s1 s2 s3 s4]         (doto sb (.append s0) (.append s1) (.append s2) (.append s3) (.append s4)))
  ([^StringBuilder sb s0 s1 s2 s3 s4 s5 & ss] (doto sb (.append s0) (.append s1) (.append s2) (.append s3) (.append s4) (.append s5))
   (apply append! sb ss)))


(defprotocol stream-protocol
  (-push! [a els])
  (commit! [a]))
(defn push! [a & els]
  ;; (assert (satisfies? html/Stream a) "expected html/Stream")
  (-push! a els))


(extend-protocol html/stream-protocol

  java.lang.StringBuilder
  ;; --------------------
  (-push! [sb els] (apply append! sb els) sb)
  (commit! [sb] (str sb))

  clojure.lang.Atom
  ;; --------------
  (-push! [a els] (swap! a -push! els) a)
  (commit! [a] (doall (deref a)))

  clojure.lang.IPersistentVector
  ;; ---------------------------
  (-push! [v els] (reduce conj v els))
  (commit! [v] v)

  clojure.lang.PersistentArrayMap
  ;; ----------------------------
  (-push! [m [el & _ :as els]]
    (reduce conj m (if (and (sequential? el) (= (count el) 2))
                     ;; if first el is map entry, assume all els are
                     els
                     ;; partition els into seq of map entries
                     (->> els (partition 2) (map vec)))))
  (commit! [m] m))


(t/deftest test-stream-protocol
  (t/testing "StringBuilder"
    (let [sb (StringBuilder.)]
      (t/is (= "foobar" (-> sb (push! "foo") (push! "b" "a" "r") (str)))                                    "should permit pushing multiple objects")
      (t/is (= "foobar123456" (-> sb (push! "1" "2" "3" "4" "5" "6") (str)))                                "should permit pushing any number of objects")
      (t/is (string? (commit! sb))                                                                          "should commit! to a String")))

  (t/testing "Atom and Vector"
    (let [sb (atom [])]
      (t/is (= ["foo" "b" "a" "r"] (-> sb (push! "foo") (push! "b" "a" "r") (deref)))                       "should permit pushing multiple objects")
      (t/is (= ["foo" "b" "a" "r" "1" "2" "3" "4" "5" "6"] (-> sb (push! "1" "2" "3" "4" "5" "6") (deref))) "should permit pushing any number of objects")
      (t/is (vector? (commit! sb))                                                                          "should commit! to a Vector")))

  (t/testing "PersistentArrayMap"
    (let [sb (array-map)]
      (t/is (= {:a 1 :b 2 :c 3} (-> sb (push! [:a 1]) (push! [:b 2] [:c 3]) (commit!)))                     "should accept map entries")
      (t/is (= {:a 1 :b 2 :c 3} (-> sb (push! [:a 1]) (push! :b 2 :c 3) (commit!)))                         "should accept key value sequence"))))


;;* Strings ----------------------------------------------------------- *;;

(defprotocol string-protocol (^String -str [x]))

(extend-protocol html/string-protocol
  clojure.lang.Keyword (-str [k] (str (symbol k)))
  clojure.lang.Ratio   (-str [r] (str (float r)))
  java.lang.String     (-str [s] s)
  java.lang.Object     (-str [o] (str o))
  nil                  (-str [_] ""))


(defn escape [^String s]
  (->> {\& "&amp;"
        \< "&lt;"
        \> "&gt;"
        \" "&quot;"
        \' "&#x27;"}
       (string/escape s)))


(def normalized-attrs
  { ;; special cases
   :default-checked "checked"
   :default-value "value"

   ;; https://github.com/facebook/react/blob/v15.6.2/src/renderers/dom/shared/HTMLDOMPropertyConfig.js
   :accept-charset "accept-charset"
   :access-key "accessKey"
   :allow-full-screen "allowfullscreen"
   :allow-transparency "allowTransparency"
   :auto-complete "autoComplete"
   :auto-play "autoplay"
   :cell-padding "cellPadding"
   :cell-spacing "cellSpacing"
   :char-set "charSet"
   :class-id "classId"
   :col-span "colSpan"
   :content-editable "contenteditable"
   :context-menu "contextMenu"
   :cross-origin "crossorigin"
   :date-time "dateTime"
   :enc-type "encType"
   :form-action "formAction"
   :form-enc-type "formEncType"
   :form-method "formMethod"
   :form-no-validate "formnovalidate"
   :form-target "formTarget"
   :frame-border "frameBorder"
   :href-lang "hrefLang"
   :http-equiv "http-equiv"
   :input-mode "inputMode"
   :key-params "keyParams"
   :key-type "keyType"
   :margin-height "marginHeight"
   :margin-width "marginWidth"
   :max-length "maxLength"
   :media-group "mediaGroup"
   :min-length "minLength"
   :no-validate "novalidate"
   :radio-group "radioGroup"
   :referrer-policy "referrerPolicy"
   :read-only "readonly"
   :row-span "rowspan"
   :spell-check "spellcheck"
   :src-doc "srcDoc"
   :src-lang "srcLang"
   :src-set "srcSet"
   :tab-index "tabindex"
   :use-map "useMap"
   :auto-capitalize "autoCapitalize"
   :auto-correct "autoCorrect"
   :auto-save "autoSave"
   :item-prop "itemProp"
   :item-scope "itemscope"
   :item-type "itemType"
   :item-id "itemId"
   :item-ref "itemRef"

   ;; https://github.com/facebook/react/blob/v15.6.2/src/renderers/dom/shared/SVGDOMPropertyConfig.js
   :allow-reorder "allowReorder"
   :attribute-name "attributeName"
   :attribute-type "attributeType"
   :auto-reverse "autoReverse"
   :base-frequency "baseFrequency"
   :base-profile "baseProfile"
   :calc-mode "calcMode"
   :clip-path-units "clipPathUnits"
   :content-script-type "contentScriptType"
   :content-style-type "contentStyleType"
   :diffuse-constant "diffuseConstant"
   :edge-mode "edgeMode"
   :external-resources-required "externalResourcesRequired"
   :filter-res "filterRes"
   :filter-units "filterUnits"
   :glyph-ref "glyphRef"
   :gradient-transform "gradientTransform"
   :gradient-units "gradientUnits"
   :kernel-matrix "kernelMatrix"
   :kernel-unit-length "kernelUnitLength"
   :key-points "keyPoints"
   :key-splines "keySplines"
   :key-times "keyTimes"
   :length-adjust "lengthAdjust"
   :limiting-cone-angle "limitingConeAngle"
   :marker-height "markerHeight"
   :marker-units "markerUnits"
   :marker-width "markerWidth"
   :mask-content-units "maskContentUnits"
   :mask-units "maskUnits"
   :num-octaves "numOctaves"
   :path-length "pathLength"
   :pattern-content-units "patternContentUnits"
   :pattern-transform "patternTransform"
   :pattern-units "patternUnits"
   :points-at-x "pointsAtX"
   :points-at-y "pointsAtY"
   :points-at-z "pointsAtZ"
   :preserve-alpha "preserveAlpha"
   :preserve-aspect-ratio "preserveAspectRatio"
   :primitive-units "primitiveUnits"
   :ref-x "refX"
   :ref-y "refY"
   :repeat-count "repeatCount"
   :repeat-dur "repeatDur"
   :required-extensions "requiredExtensions"
   :required-features "requiredFeatures"
   :specular-constant "specularConstant"
   :specular-exponent "specularExponent"
   :spread-method "spreadMethod"
   :start-offset "startOffset"
   :std-deviation "stdDeviation"
   :stitch-tiles "stitchTiles"
   :surface-scale "surfaceScale"
   :system-language "systemLanguage"
   :table-values "tableValues"
   :target-x "targetX"
   :target-y "targetY"
   :view-box "viewBox"
   :view-target "viewTarget"
   :x-channel-selector "xChannelSelector"
   :xlink-actuate "xlink:actuate"
   :xlink-arcrole "xlink:arcrole"
   :xlink-href "xlink:href"
   :xlink-role "xlink:role"
   :xlink-show "xlink:show"
   :xlink-title "xlink:title"
   :xlink-type "xlink:type"
   :xml-base "xml:base"
   :xmlns-xlink "xmlns:xlink"
   :xml-lang "xml:lang"
   :xml-space "xml:space"
   :y-channel-selector "yChannelSelector"
   :zoom-and-pan "zoomAndPan"})


;; https://github.com/facebook/react/blob/master/src/renderers/dom/shared/CSSProperty.js
(def unitless-css-props
  (into #{}
        (for [key ["animation-iteration-count" "box-flex" "box-flex-group" "box-ordinal-group" "column-count" "flex" "flex-grow" "flex-positive" "flex-shrink" "flex-negative" "flex-order" "grid-row" "grid-column" "font-weight" "line-clamp" "line-height" "opacity" "order" "orphans" "tab-size" "widows" "z-index" "zoom" "fill-opacity" "stop-opacity" "stroke-dashoffset" "stroke-opacity" "stroke-width"]
              prefix ["" "-webkit-" "-ms-" "-moz-" "-o-"]]
          (str prefix key))))


(defn normalize-attr-key ^String [key]
  (or (normalized-attrs key)
      (when (.startsWith (name key) "on")
        (-> (name key) (string/lower-case) (string/replace "-" "")))
      (name key)))


(defn normalize-css-key [k]
  (-> (-str k)
      (string/replace #"[A-Z]" (fn [ch] (str "-" (string/lower-case ch))))
      (string/replace #"^ms-" "-ms-")))


(defn normalize-css-value [key value]
  (cond
    (contains? unitless-css-props key) (escape (string/trim (-str value)))
    (number? value) (str value (when (not= 0 value) "px"))
    :else (escape (string/trim (-str value)))))


;;* Hierarchies ------------------------------------------------------- *;;


;; TODO categorize tags e.g. as per MDN to target groups of tags with methods and hierarchies?
(def tags
  (->>
   #{"html" "base" "head" "link" "meta" "style" "title" "body" "address" "article" "aside" "footer" "header" "h1" "h2" "h3" "h4" "h5" "h6" "hgroup" "main" "nav" "section" "blockquote" "dd" "div" "dl" "dt" "figcaption" "figure" "hr" "li" "ol" "p" "pre" "ul" "a" "abbr" "b" "bdi" "bdo" "br" "cite" "code" "data" "dfn" "em" "i" "kbd" "mark" "q" "rb" "rp" "rt" "rtc" "ruby" "s" "samp" "small" "span" "strong" "sub" "sup" "time" "u" "var" "wbr" "area" "audio" "img" "map" "track" "video" "embed" "iframe" "object" "param" "picture" "source" "canvas" "noscript" "script" "del" "ins" "caption" "col" "colgroup" "table" "tbody" "td" "tfoot" "th" "thead" "tr" "button" "datalist" "fieldset" "form" "input" "label" "legend" "meter" "optgroup" "option" "output" "progress" "select" "textarea" "details" "dialog" "menu" "summary" "slot" "template"}
   (reduce #(assoc %1 (clojure.core/keyword %2) %2) {})))


(defn tag?
  ([tag #_in category] (throw "implement me"))
  ([tag] (get tags tag)))


;; TODO defmulti simply expects hierarchy it receives to be a reference type, so needn't be an atom
;; - var will do just as well. So we leave these as vars, not atoms and pass e.g. (var
;; tag-hierarchy) to relevent defmulti. isa! and are! would then use alter-var-root instead of swap!
(def tag-hierarchy (atom (make-hierarchy)))
(def attribute-hierarchy (atom (make-hierarchy)))


(defn isa! [hierarchy tag parent-tag] (swap! hierarchy derive tag parent-tag))
(defn are! [hierarchy tags parent-tag] (swap! hierarchy #(reduce (fn [h t] (derive h t parent-tag)) % tags)))
(comment
  (as-> :base tag (isa! tag-hierarchy tag ::noclose))
  (as-> :area tag (isa! tag-hierarchy tag ::noclose))
  (as-> [:base :area] tags (are! tag-hierarchy tags ::noclose))
  ;; comment
  )


(as-> #{"area" "base" "br" "col" "command" "embed" "hr" "img" "input" "keygen" "link" "meta" "param" "source" "track" "wbr"} tags
  (map clojure.core/keyword tags)
  (are! tag-hierarchy tags ::noclose))


;;* Records ----------------------------------------------------------- *;;


(defrecord dangerous-record [string])
(def dangerous dangerous-record)
(defmethod make html/dangerous [_ html] (dangerous-record. (-str html)))


(defrecord element-record [tag id classes attrs children])
(def element element-record)
(defmethod make html/element
  ([_ tag id classes attrs children]
   (assert (tag? tag) (format "Expected a valid HTML tag but got %s" tag))
   (if-let [html! (:dangerouslySetInnerHTML attrs)]
     (element-record. tag id classes (dissoc attrs :dangerouslySetInnerHTML) (list (make dangerous (or (:__html html!) (first children)))))
     (element-record. tag id classes attrs children))))


;;* Multi: keyword ---------------------------------------------------- *;;


(defmulti keyword
  "May produce any value that html/Protocol knows how to handle but will
  usually return either html/element record or [tag ...] vector."
  (fn [tag & _]
    (assert
     (keyword? tag)
     (format "html/keyword: expected a keyword but got %s" tag))
    tag)
  :default ::tag
  :hierarchy tag-hierarchy)


(defmethod keyword ::tag [tag _id {:keys [selector attrs children]}]
  (let [selector (or selector (css/parse tag))
        {:keys [tag id class]} (get selector :split)
        id (or (:string id) (:id attrs))
        classes (->> (get attrs :class)
                     (map #(make css/selector % :class))
                     ;; => extract attrs classes and concat them to the end so they have highest priority
                     (concat [(make css/selector selector :tag)
                              (make css/selector selector :class)
                              (make css/selector selector :id)])
                     ;; => split html element head into selectors in reverse priority order
                     (filter some?)
                     (mapcat css/classes)
                     ;; => recursively collect classes installed via dynamic css/*css* binding
                     (prelude/nilify empty?))]
    (make html/element
          #_tag (or (:raw tag) :div)
          id classes
          #_attrs (dissoc attrs :class :id)
          children)))


;;* Multi: push-element! ---------------------------------------------- *;;


(declare render)


(defmulti push-element! (fn [_sb {tag :tag}] tag)
  :default ::tag
  :hierarchy tag-hierarchy)


(defmethod push-element! ::tag [sb {:keys [tag id classes attrs children] :as el}]
  (as-> sb sb
    (push! sb "<" (-str tag))
    (render (cond-> attrs id (assoc :id id) classes (assoc :class classes)) sb)
    (push! sb ">")
    (render children sb)
    (push! sb "</" (-str tag) ">")))


(defmethod push-element! ::noclose [sb {:keys [tag id classes attrs children] :as el}]
  (assert (empty? children) (format "Tag %s is a \"noclose\" tag and must not have children" tag))
  (as-> sb sb
    (push! sb "<" (-str tag))
    (render (cond-> attrs id (assoc :id id) classes (assoc :class classes)) sb)
    (push! sb "/>")))


;;* Multi: push-attribute! -------------------------------------------- *;;


(defmulti  push-attribute! (fn [_sb [attr _]] attr)
  :default ::attribute
  :hierarchy  attribute-hierarchy)


(defmethod push-attribute! ::attribute [sb [k v]]
  (if (not v) sb
      ;; Ignore attributes with false and nil values
      (let [k (normalize-attr-key k)
            v (if (true? v) "" v)]
        (if (instance? StringBuilder sb)
          ;; is this faster cause we don't create intermediate strings?
          (push! sb " " (-str k) "=" "\"" (escape (-str v)) "\"")
          (push! sb (str " " (-str k) "=") (str "\"" (escape (-str v)) "\""))))))


(defmethod push-attribute! :class [sb [_ classes]]
  (if (or (not classes) (empty? classes)) sb
      ;; Ignore empty, false and nil classes
      (push! sb
             " class="
             ;; TODO avoid intermediate strings if StringBuilder?
             (str "\"" (->> classes (map (comp escape -str)) (interpose " ") (apply str)) "\""))))


(defn push-style! [sb k v]
  (let [key (normalize-css-key k)
        val (normalize-css-value key v)]
    (push! sb key ":" val ";")))


(defn chop-last-semicolon! [sb]
  (let [i (dec (.length sb))]
    (if (and (pos? i) (= (.charAt sb i) \;))
      (.deleteCharAt sb i)
      sb)))


(t/deftest test-chop-last-semi
  (t/is (= "a" (str (chop-last-semicolon! (StringBuilder. "a;")))))
  (t/is (= ""  (str (chop-last-semicolon! (StringBuilder. "")))))
  (t/is (= "a" (str (chop-last-semicolon! (StringBuilder. "a"))))))


(defmethod push-attribute! :style [sb [_ styles]]
  (if (or (not styles) (empty? styles)) sb
      ;; Ignore empty, false or nil styles
      (push! sb " style=" (str "\"" (chop-last-semicolon! (reduce-kv push-style! (StringBuilder.) styles)) "\""))))


(defmethod push-attribute! :dangerouslySetInnerHTML [sb attr] sb)
(comment
  ;; TODO alternatively we could use hierarchy for :nop attributes
  (-> :dangerouslySetInnerHTML (isa! attribute-hierarchy ::nop))
  (defmethod push-attribute! ::nop [sb attr] sb)
  ;; comment
  )


;;* Element protocol -------------------------------------------------- *;;


(defprotocol element-protocol (tagged [tag & _]))


(extend-protocol html/element-protocol

  clojure.lang.Keyword
  ;; -----------------
  (tagged [tag attrs children]
    (let [selector (css/parse tag)]
      (keyword
       #_kwtag (or (some-> selector :split :class :raw)
                   (some-> selector :split :tag :raw)
                   ;; TODO or should we default to ::div in case we decide to supply default
                   ;; behavior for certain tags e.g. div?
                   :div)
       #_kwid (or (some-> selector :split :id :string)
                  (some-> attrs :id))
       {:selector selector
        :attrs attrs
        :children children})))

  clojure.lang.IFn
  ;; -------------
  (tagged [tag attrs children] (throw "implement me"))

  clojure.lang.Symbol
  ;; ----------------
  (tagged [tag attrs children] (throw "implement me")))


;;* Html protocol ----------------------------------------------------- *;;


(defprotocol protocol
  (render [el] [el sb] [el sb commit?] "Render HTML out of \"hiccup\" style tree of vectors. sb
  argument defines what HTML is and defaults to a StringBuilder. It may also be (atom []) or [] to
  allow fine-grain debugging")
  (expand [el] "Perform html/keyword expansion and return \"hiccup\" style tree of vectors. This
  method is not part of HTML rendering pipeline but helps coarse-grain debugging."))


(extend-protocol html/protocol

  clojure.lang.IPersistentVector
  ;; ---------------------------
  (render
    ([vec]
     ;; assuming only ever called by "user" to start rendering, so we commit the result
     (render vec (StringBuilder.) :commit))
    ([vec sb]
     ;; typically called recursively - callers must commit themselves when required
     (render vec sb false))
    ([[tag attrs & children] sb commit?]
     (let [result-sb
           (render
            (tagged tag
                    #_attrs (if (map? attrs) attrs {})
                    #_children (if (or (map? attrs) (not attrs)) children (cons attrs children)))
            sb)]
       (if commit?
         (commit! result-sb)
         result-sb))))
  (expand [[tag attrs & children]]
    (expand
     (tagged tag
             #_attrs (if (map? attrs) attrs {})
             #_children (if (or (map? attrs) (not attrs)) children (cons attrs children)))))

  clojure.lang.ISeq
  ;; ---------------
  (render [els sb]
    (loop [els els sb sb]
      (cond
        (seq els) (recur (rest els) (render (first els) sb))
        :loop-done sb)))
  (expand [els]
    (map expand els))


  clojure.lang.PersistentArrayMap
  ;; ---------------------------- attrs
  (render [attrs sb]
    (condp instance? sb
      StringBuilder    (reduce push-attribute! sb attrs)
      PersistentVector (->> attrs (reduce push-attribute!       {})  (commit!) (push! sb))
      Atom             (->> attrs (reduce push-attribute! (atom {})) (commit!) (push! sb))))
  (expand [attrs] attrs)

  element-record
  ;; -----------
  (render [el sb]
    (condp instance? sb
      StringBuilder    (push-element! sb el)
      PersistentVector (->> el (push-element!       [])  (commit!) (push! sb))
      Atom             (->> el (push-element! (atom [])) (commit!) (push! sb))))
  (expand [{:keys [tag id classes attrs children] :as el}]
    (let [attrs (cond-> attrs
                  id (assoc :id id)
                  classes (assoc :class classes))]
      (reduce
       (fn [el child] (if (seq? child) (reduce conj el child) (conj el child)))
       [tag attrs]
       (map expand children))))

  dangerous-record
  ;; -------------
  (render [dangerous sb]
    (push! sb (get dangerous :string)))
  (expand [dangerous]
    (get dangerous :string))

  java.lang.String
  ;; -------------
  (render [s sb]
    (push! sb (escape s)))
  (expand [s] s)

  java.lang.Object
  ;; -------------
  (render [o sb]
    (render (str o) sb))
  (expand [o] o)

  nil
  ;; -------------
  (render [n sb] sb)
  (expand [n] n))


(when t/*load-tests*

  (defn debug
    ([html]
     (debug render html))
    ([render html]
     (let [html (render html)]
       (println html)
       html)))


  (defn css []
    (make css/css
          ::#id      {:classes [:foo] :border "1px solid red"}
          :body      {:width 400 :classes ['body]}
          ::foo      {:classes ['foo]}
          ::on       {:classes '[pl-1 pr-1]}
          ::input    {:classes '[::on border text-gray-800]}
          ::textarea {:classes [::input 'w-full]}))


  (defmethod t/assert-expr 'html? [msg [_ input-vec expected-html :as form]]
    `(binding [css/*css* (css)]
       (let [actual# (render ~input-vec)
             expected# ~expected-html
             test# (= actual# expected#)]
         (if test#
           (t/do-report {:type :pass :message ~msg :expected expected# :actual actual#})
           ;; TODO on failure produce msg with diff:
           ;; https://github.com/tonsky/rum/blob/gh-pages/test/rum/test/server_render.cljc#L298
           (t/do-report {:type :fail :message ~msg :expected expected# :actual actual#})))))


  (defmacro renders? [form html & msgs]
    (let [html? 'html?]
      `(t/testing (str ~@msgs)
         (t/is (~html? ~form ~html))))))


(t/deftest test-rendering

  (t/testing "simple HTML"
    (-> [:div]                                               (renders? "<div></div>"))
    (-> [:foo]                                               (renders? "<div class=\"foo\"></div>"))
    (-> [:div "a" "b"]                                       (renders? "<div>ab</div>"))
    (-> [:div {} "body"]                                     (renders? "<div>body</div>"))
    (-> [:div {:foo "bar"} "body"]                           (renders? "<div foo=\"bar\">body</div>"))
    (-> [:div {:foo "bar"} [:span "a"] "b" [:foo "c"]]       (renders? "<div foo=\"bar\"><span>a</span>b<div class=\"foo\">c</div></div>"))
    (t/testing "with list content"
      (-> [:div nil]                                         (renders? "<div></div>"))
      (-> [:div '("a" "b")]                                  (renders? "<div>ab</div>"))
      (-> [:div '()]                                         (renders? "<div></div>"))
      (-> [:div '() "b"]                                     (renders? "<div>b</div>"))
      (-> [:div (list '() "b")]                              (renders? "<div>b</div>")))
    (t/testing "with nested content"
      (-> [:div [:div "b"]]                                  (renders? "<div><div>b</div></div>"))
      (-> [:div "a" [:div "b"]]                              (renders? "<div>a<div>b</div></div>"))
      (-> [:div "a" (list [:div "b"] "c") (list "d")]        (renders? "<div>a<div>b</div>cd</div>"))))


  (t/testing "attribute"
    (-> [:div {:data-attr-ibute "b"}]                        (renders? "<div data-attr-ibute=\"b\"></div>"               "should not touch data-*"))
    (-> [:div {:aria-checked "c"}]                           (renders? "<div aria-checked=\"c\"></div>"                  "should not touch aria-*"))
    (-> [:div {:form-enc-type "text/plain"}]                 (renders? "<div formEncType=\"text/plain\"></div>"          "should remove dashes"))
    (-> [:div {:allow-full-screen true}]                     (renders? "<div allowfullscreen=\"\"></div>"                "should convert true value to \"\""))
    (-> [:div {:href "/a=b&c=d"}]                            (renders? "<div href=\"/a=b&amp;c=d\"></div>"               "should escape :href"))
    (-> [:div {:checked false :data nil}]                    (renders? "<div></div>"                                     "should ignore nil and false attrs"))
    (-> [:div {:style {:fontWeight 10 "msFlex" 1}}]          (renders? "<div style=\"font-weight:10;-ms-flex:1\"></div>" "should convert react-style properties"))
    (t/testing "style"
      (-> [:div {:style {:background-image "url(\"123\")"}}] (renders? "<div style=\"background-image:url(&quot;123&quot;)\"></div>" "should escape double quotes"))
      (-> [:div {:style {:background-image "url('123')"}}]   (renders? "<div style=\"background-image:url(&#x27;123&#x27;)\"></div>" "should escape single quotes"))
      (-> [:div {:style {:line-height      24}}]             (renders? "<div style=\"line-height:24\"></div>"                        "should not add 'px' to unitless property"))
      (-> [:div {:style {:-webkit-box-flex 3}}]              (renders? "<div style=\"-webkit-box-flex:3\"></div>"                    "should handle prefixed unitless property"))
      (-> [:div {:style {:margin-top       17}}]             (renders? "<div style=\"margin-top:17px\"></div>"                       "should add 'px'"))
      (-> [:div {:style {:margin-left      0}}]              (renders? "<div style=\"margin-left:0\"></div>"                         "should not add 'px' when 0"))
      (-> [:div {:style {:padding-bottom   "1em"}}]          (renders? "<div style=\"padding-bottom:1em\"></div>"                    "should not add 'px' when unit specified"))
      (-> [:div {:style {:text-align       " left  "}}]      (renders? "<div style=\"text-align:left\"></div>"                       "should trim non-numeric values"))
      (-> [:div {:style {:flex-grow        " 1  "}}]         (renders? "<div style=\"flex-grow:1\"></div>"                           "should trim unitless value"))))


  (t/testing "keyword tagged HTML"
    (-> [::foo]                                              (renders? "<div class=\"foo fullmeta.html/foo\"></div>"                                                                                       "should default to :div tag but inject namespaced class"))
    (-> [::#id]                                              (renders? "<div id=\"fullmeta.html/id\" class=\"foo\"></div>"                                                                                 "should inject id and expand its classes from dynamic CSS"))
    (-> [::foo#id]                                           (renders? "<div id=\"fullmeta.html/id\" class=\"foo fullmeta.html/foo foo\"></div>"                                                             "should inject both id and namespaced class"))
    (-> [::foo#id {:class [::on]}]                           (renders? "<div id=\"fullmeta.html/id\" class=\"foo fullmeta.html/foo foo pl-1 pr-1 fullmeta.html/on\"></div>"                                    "should expand inline classes"))
    (-> [::textarea {:class ['bar]}]                         (renders? "<textarea class=\"pl-1 pr-1 fullmeta.html/on border text-gray-800 fullmeta.html/input w-full fullmeta.html/textarea bar\"></textarea>" "should recursively expand classes"))
    (-> [:div {:class [::foo 'bar "baz"]}]                   (renders? "<div class=\"foo fullmeta.html/foo bar baz\"></div>"                                                                               "should admit kw, sym, string classes"))
    (t/testing "should treat tag and namespaced tag differently when expanding classes"
      (-> [:input {:class ['bar]}]                           (renders? "<input class=\"bar\"/>"))
      (-> [::input {:class ['bar]}]                          (renders? "<input class=\"pl-1 pr-1 fullmeta.html/on border text-gray-800 fullmeta.html/input bar\"/>"))))


  (t/testing "inner HTML dangerously"
    (-> [:style {:dangerouslySetInnerHTML {:__html (css/render (css))}}] (renders? "<style>#fullmeta\\.html\\/id{border:1px solid red;}body{width:400px;}</style>"))
    (-> [:style {:dangerouslySetInnerHTML true} (css/render (css))]      (renders? "<style>#fullmeta\\.html\\/id{border:1px solid red;}body{width:400px;}</style>")))


  (t/testing "full HTML page"
    (-> [:html
         [:head
          [:style {:dangerouslySetInnerHTML {:__html (css/render (css))}}]
          [:title "title here"]]
         [:body
          [:form#id {:action "#" :method "post" :class ['bar ::on]}
           [::input {:placeholder "input here"}]]]]
        (renders?
         (str "<html>"
              "<head><style>#fullmeta\\.html\\/id{border:1px solid red;}body{width:400px;}</style><title>title here</title></head>"
              "<body class=\"body\">"
              "<form action=\"#\" method=\"post\" id=\"id\" class=\"bar pl-1 pr-1 fullmeta.html/on\">"
              "<input placeholder=\"input here\" class=\"pl-1 pr-1 fullmeta.html/on border text-gray-800 fullmeta.html/input\"/>"
              "</form>"
              "</body>"
              "</html>")))))

#_
(t/run-tests)


;;* Components -------------------------------------------------------- *;;


(defmethod keyword ::html/style [_tag _id {css :attrs}]
  (let [css (cond
              (string? css) css
              (empty? css)  (->> css/*css* (make css/css) (css/render))
              :else         (->> css       (make css/css) (css/render)))]
    [:style {:dangerouslySetInnerHTML {:__html css}}]))


(defn style
  ([]    [::html/style css/*css*])
  ([css] [::html/style css]))


;; TODO Install css via attribute e.g. [:tag {:css css} children] ensures that this tree and its
;; children are rendered with (binding [css/*css* (combine css/*css* css)] ...) where combine is a
;; way to combine CSSes. Simplest and obvious are: replace *css* with css, "conj" css onto
;; *css*, "cons" css onto *css*, "merge" with css rules replacing matching *css* ones a-la Clojure's
;; merge. First see which (if at all) combinators actually come up in development.


(defn with-css [css html] :TODO?)


(t/deftest test-style-component
  (-> [::html/style (css)] (renders? "<style>#fullmeta\\.html\\/id{border:1px solid red;}body{width:400px;}</style>"))
  (-> [::html/style]       (renders? "<style>#fullmeta\\.html\\/id{border:1px solid red;}body{width:400px;}</style>"))
  (-> [::html/style (css)] (renders? (render (style (css))))))
