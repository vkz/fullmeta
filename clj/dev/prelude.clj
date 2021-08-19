(ns prelude
  (:require [clojure.edn            :as edn]
            [clojure.pprint         :as pp]
            [clojure.java.classpath :as cp]
            [clojure.java.io        :as io]
            [clojure.string         :as string]
            [clojure.reflect        :as reflect]
            [clojure.tools.namespace.find :as ns.find]
            [clojure.tools.namespace.file :as ns.file]
            [clojure.tools.namespace.dir :as ns.dir]
            [clojure.tools.namespace.repl :as ns.repl]
            [clojure.tools.namespace.track :as ns.track]
            [clojure.tools.namespace.reload :as ns.reload])
  (:import javax.tools.ToolProvider))


(defn classpath
  "Get current classpath as seq of File objects"
  []
  (map str (cp/system-classpath)))


(defn deps-edn
  "Read in our deps.edn"
  []
  (->>
   (slurp "deps.edn")
   (edn/read-string)))


(defn reflect
  "Helper to reflect, print and return all members of a Java object."
  ([obj]
   (reflect obj :members))
  ([obj property]
   (let [members (get (reflect/reflect obj) property)]
     (pp/print-table members)
     members)))

#_(reflect (Path/of "store/bar" (into-array String nil)))


;; Refresh "changed" namespaces in source dependency order with clojure.tools.namespace
(defn refresh [& targets]
  (let [targets (->> targets (map #(io/as-file %)) (filter #(.exists %)))
        {:keys [dirs files]} (group-by #(if (.isDirectory %) :dirs :files) targets)
        scan-opts {:platform ns.find/clj}
        refresh-tracker (-> (ns.track/tracker)
                            (ns.dir/scan-dirs dirs scan-opts)
                            (ns.dir/scan-files files scan-opts)
                            ;; remove-disabled is private hence the var
                            ((var ns.repl/remove-disabled)))]
    ((var ns.repl/print-pending-reloads) refresh-tracker)
    #_(pp/pprint refresh-tracker)
    (ns.reload/track-reload refresh-tracker)))

(comment
  (refresh "clj/src/main.clj")
  (refresh "clj/dev/make.clj")
  (refresh "clj/src/fullmeta/cgi.clj")
  (refresh "clj/src/fullmeta")
  (refresh "clj/dev"))


;; NOTE Javadoc for javax.tools.ToolProvider isn't obvious, so I inferred API from this example
;; https://github.com/EwenG/badigeon/blob/master/src/badigeon/javac.clj#L56-L59
(defn javac
  ([]
   (javac "clj/src"))
  ([dir]
   (let [files (when-let [dir (io/as-file dir)]
                 (when (.isDirectory dir)
                   (->> (file-seq dir)
                        (map str)
                        (filter #(string/ends-with? % ".java"))
                        (sort))))
         compiler (javax.tools.ToolProvider/getSystemJavaCompiler)
         ;; out (java.io.ByteArrayOutputStream.)
         ;; err (java.io.ByteArrayOutputStream.)
         ]
     (when (seq files)
       (if (zero? (.run compiler
                        #_stdin nil     ; defaults to System.in
                        #_stdout nil    ; defaults to System.out
                        #_stderr nil    ; defaults to System.err
                        (into-array String
                                    (reduce conj
                                            ["-cp" (->> (classpath)
                                                        (interpose ":")
                                                        (apply str))]
                                            ;; "-d" "clj/classes"
                                            ;; "clj/src/fullmeta/MultiFn.java"
                                            files))))
         (->> files
              (map #(hash-map :source % :target (string/replace-first % #".java$" ".class")))
              (clojure.pprint/print-table [:source :target])
              (concat (cons :compiled  files)))
         (do
           ;; TODO report errors
           #_(println (str out))
           #_(println (str err))
           (println "oops")))))))


(comment
  ;; Compile all .java files under clj/src and place .class files in the same dir
  (javac)
  ;; comment
  )
