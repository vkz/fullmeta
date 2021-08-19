;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((nil . ((eval . (progn
                   (require 'cider)
                   (cider-register-cljs-repl-type
                    'fullmeta-cljs-repl
                    (format "%s"
                            ;; Clojure code to execute
                            '(do
                              ;; (user/-main)
                              (require 'make)
                              (make/make :cljs/repl))))))
         (cider-default-cljs-repl . fullmeta-cljs-repl)))

 (clojure-mode . ((cider-clojure-cli-global-options . "-A:dev")
                  (fill-column . 100))))
