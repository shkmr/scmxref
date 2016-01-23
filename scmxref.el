;;;
;;;    Emacs functions to interact with scmxref-makiki
;;;    load from .emacs 
;;;  

;;;  1) In one of your Gauche program buffer,
;;;
;;;         M-x run-scmxref
;;;
;;;     will start scmxref-makiki and your web browser will be
;;;     loaded with ``Directory listing: . '' page.
;;;     If you asked for passcode it will be 666  (See start-scmxref below).
;;;
;;;  2) In the web browser, hit (add) or (AddAll) to build xref index.
;;;
;;;  3) Then C-c C-f (gauche-man) on your global variable.
;;;

;;
(defvar scmxref-process nil)

(defun scmxref-filter (proc str)

  (cond ((string-match  "^Visit http://" str)
         (let ((len (length str)))
           (if (string= "\n" (substring str (- len 1) len))
               (browse-url (concat (substring str 6 (- len 1)) "browse/"))
             (browse-url (concat (substring str 6 len) "browse/" )))))
        ((string-match "^Close request received." str)
         (message "scmxref server closed")))

  ;;
  ;; ordinary-insertion-filter as described in Emacs Lisp manual.
  ;;
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((moving (= (point) (process-mark proc))))
        (save-excursion
          ;; Insert the text, advancing the process marker.
          (goto-char (process-mark proc))
          (insert str)
          (set-marker (process-mark proc) (point)))
        (goto-char (process-mark proc))))))

(defun start-scmxref ()
  (let* ((k "666") ;; Todo: generate nice passcode
         (p (start-process "scmxref-process" "*scmxref*"
                           ;; "gosh" "-I." ; "-fload-verbose"
                           "scmxref-makiki" "-k" k)))
    (set-process-filter p 'scmxref-filter)
    (setq scmxref-process p)
    (message "SCMXREF: Passcode is %s" k)))

(defun scmxref-sentinel (p e)
  (cond ((string-match "^finished" e)
         (start-scmxref))))

(defun start-unique-scmxref ()
  (if (not (process-live-p scmxref-process))
      (start-scmxref)
    (set-process-sentinel scmxref-process 'scmxref-sentinel)
    (browse-url "http://localhost:8888/close")))

;;
(defun run-scmxref ()
  (interactive)
  ;;  Port number has to match with the one given to scmxref-makiki.
  ;;  default is 8888.
  (setq gauche-man-base "http://localhost:8888/find/?name=")
  (server-start) ; XXX Do I need to check if emacs server is already run before this?
  (start-unique-scmxref))

(defun kill-scmxref ()
  (interactive)
  (when (process-live-p scmxref-process)
    (browse-url "http://localhost:8888/close")
    (display-buffer "*scmxref*")))
;;
(defun scmxref-goto (file lineno)
  (let ((buf (find-file-other-window file)))
    (goto-line lineno)
    (other-window -1)
    (select-frame-set-input-focus (selected-frame) t)
    ;; emacsclient displays following string to its output,
    ;; which is *scmxref* buffer.
    (format "%s at %d" file lineno)))

;;;
;;;    gauche-man
;;;
(setq gauche-man-base "http://practical-scheme.net/gauche/man/?l=en&p=")

(defun gauche-man ()
  (interactive)
  (let ((key (if (use-region-p)
                 (buffer-substring-no-properties (region-beginning)
                                                 (region-end))
               (thing-at-point 'symbol))))
    (browse-url (concat gauche-man-base key))))

;;;
;;;
;;;
(add-hook 'scheme-mode-hook
          (function
           (lambda ()
             (define-key scheme-mode-map "\C-c\C-f" 'gauche-man))))
;;;
;;;
;;;
;;(put 'respond/ok 'scheme-indent-function 1)

;;; EOF
