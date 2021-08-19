(ns www.foo)

(defn ^:cgi bar [request]
  [`::bar request])


(defn ^:cgi cgi [request]
  [`::root request])
