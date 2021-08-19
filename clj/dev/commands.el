;; -*- lexical-binding: t; fill-column: 100; -*-

(require 'nrepl-client)
(require 'cider-connection)
(require 'sesman)

;; TODO Should I re-compile CLJS as part of the reload before
;; browser-reload is even called or use watch dir instead?
(defun fullmeta-reload ()
  (interactive)
  ;; Maybe invoke it in just the first repl? Although how likely are
  ;; we to have multiple buffers?
  (cider-map-repls :cljs
    (lambda (connection)
      ;; could also use nrepl-sync-request:eval
      (nrepl-request:eval
       ;; input
       (format "%s" '(browser-reload))
       ;; callback
       (lambda (response)
         ;; response is an alist, see e.g.
         ;; `nrepl-make-response-handler' for more details
         (setq fullmeta-reload-response response)
         (message "RELOADING..."))
       ;; connection
       connection
       ;; ns
       "cljs.user"))))


(defun fullmeta-toggle-reload-after-save ()
  (interactive)
  (if (member #'fullmeta-reload after-save-hook)
      (progn
        (remove-hook 'after-save-hook #'fullmeta-reload 'buffer-local)
        (message "RELOAD on safe DISABLED"))
    (when (and buffer-file-name
               (member
                major-mode
                '(clojure-mode
                  clojurescript-mode)))
      (add-hook 'after-save-hook #'fullmeta-reload nil 'buffer-local)
      (message "RELOAD on safe ENABLED"))))


(defun fullmeta-reload-advice (&rest args)
  (fullmeta-reload))

(defun fullmeta-toggle-reload-after-eval ()
  (interactive)
  (if (advice-member-p #'fullmeta-reload-advice #'cider-interactive-eval)
      (advice-remove #'cider-interactive-eval #'fullmeta-reload-advice)
    (advice-add #'cider-interactive-eval :after #'fullmeta-reload-advice)))


(defun fullmeta-reload-or-toggle (toggle)
  (interactive "p")
  (cl-case toggle
    ((1) (call-interactively #'fullmeta-reload))
    ((4) (call-interactively #'fullmeta-toggle-reload-after-save))
    ((16) (call-interactively #'fullmeta-toggle-reload-after-eval))))


(bind-key "C-c C-r SPC" #'fullmeta-reload-or-toggle)
(bind-key "C-c r" #'fullmeta-reload-or-toggle)


(defun fullmeta-kill-repls (arg)
  (interactive "p")
  (cl-case arg
    ((1)
     (message "Closing all CIDER sessions ...")
     (cl-dolist (repl (sesman-current-session 'CIDER))
       (cider--close-connection repl)))
    ((4)
     (message "Restarting all CIDER sessions ...")
     (sesman-restart 'all))))


;;* fullmeta.html debug commands --------------------------------------- *;;
(require 'cider-eval)
(require 'mhtml-mode)

;; NOTE franken-function stitched together by looking at `cider-pprint-eval-last-sexp' and
;; `cider-eval-last-sexp' and following their implementation. This can be generalized and simplified
;; so that we may connect Clojure and Emacs environments whenever and however we want. We can then
;; create ad-hoc eval commands that handle eval results (value, stdout) in custom manner with full
;; power of Emacs modes at our disposal.
;;
;; Obviously we could've gone with a more "Unix" style approach where in Clojure we capture stdout
;; some file then have Emacs command read that file. This'd do the trick in basic situation.
;;
;; With similar approach we could also "extend" or implement our own `cider-pprint-eval-last-sexp'
;; one that dispatches the returned value e.g. via :tag or smth to handler on Emacs side.
(defun fullmeta-pprint-html-form ()
  "Pretty print FORM in popup buffer."
  (interactive)
  (let* ((form (cider-last-sexp 'bounds))
         (buffer (current-buffer))
         (place form)
         (beg (car-safe place))
         (end (or (car-safe (cdr-safe place)) place))
         (beg (when beg (copy-marker beg)))
         (end (when end (copy-marker end)))
         (fringed nil)
         (handler (nrepl-make-response-handler
                   buffer

                   ;; handle value: show value in overlay and minibuffer
                   (lambda (_buffer value)
                     (if beg
                         (unless fringed
                           (cider--make-fringe-overlays-for-region beg end)
                           (setq fringed t))
                       (cider--make-fringe-overlay end))
                     (cider--display-interactive-eval-result value end))

                   ;; handle stdout: pretty-print stdout (html) to *fullmeta-html-output* buffer
                   (lambda (_buffer out)
                     (let ((output-buffer (or (get-buffer "*fullmeta-html-output*")
                                              (cider-popup-buffer "*fullmeta-html-output*" t 'mhtml-mode 'ancillary))))
                       (with-current-buffer output-buffer
                         (setq buffer-read-only nil)
                         (erase-buffer))
                       (cider-emit-into-popup-buffer output-buffer out)
                       (pop-to-buffer output-buffer)
                       ;; I think cider-emit is async, so we end up with unbalanced tree and error
                       ;; when pretty print too soon. Is there a way to wait for all output printed?
                       (sleep-for 3)
                       (sgml-pretty-print (point-min) (point-max))
                       (setq buffer-read-only t)))

                   ;; handle stderr: default cider behavior
                   (lambda (_buffer err)
                     (cider-emit-interactive-eval-err-output err))
                   nil
                   nil
                   nil
                   (lambda (buffer warning)
                     (cider-emit-into-popup-buffer buffer warning 'font-lock-warning-face t)))))
    (with-current-buffer buffer
      (cider-interactive-eval (when (stringp form) form)
                              handler
                              (when (consp form) form)
                              (cider--nrepl-print-request-map fill-column)))))
