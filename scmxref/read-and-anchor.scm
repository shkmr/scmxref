(select-module scmxref)
;;;
;;;
;;;

;; options
(define anchor-all-lines      (make-parameter #f))
(define anchor-file-extension (make-parameter ".html"))
(define place-goto-anchor     (make-parameter #f))
(define gauche-man-base       (make-parameter "http://practical-scheme.net/gauche/man/?l=en&p="))

;;;
;;;  read-and-anchor
;;;
(define (disp x)
  (cond ((not (char? x))
         (with-input-from-string (x->string x)
           (lambda ()
             (port-for-each disp read-char))))
        ((char=? #\< x)
         (display "&lt;"))
        ((char=? #\> x)
         (display "&gt;"))
        ((char=? #\& x)
         (display "&amp;"))
        (else
         (display x))))

(define symbol-initial  #[A-Za-z!$%&*/:<=>?\^~+\-])
(define symbol-char-set #[\w!$%&*/:<=>?\^~+\-@.])

(define (read-string-w/o-escape ch lis)
  (cond ((eof-object? ch) (error "EOF inside string"))
        ((char=? #\"  ch) (apply string (reverse lis)))
        ((char=? #\\  ch)
         (let ((x (read-char)))
           (read-string-w/o-escape (read-char)
                                   (cons x (cons ch lis)))))
        (else
         (read-string-w/o-escape (read-char)
                                 (cons ch lis)))))

(define (read-symbol ch lis)

  (define (list->symbol lis)
    (string->symbol (apply string (reverse lis))))

  (cond ((eof-object? ch) (list->symbol lis))
        ((char-set-contains? symbol-char-set ch)
         (read-char)
         (read-symbol (peek-char) (cons ch lis)))
        (else
         (list->symbol lis))))

(define (do-newline)
  (newline)
  (if (anchor-all-lines)
    (anchor-line))
  'newline)

(define (do-sharp lvl)

  (define (do-debug)
    (disp (read-char)) ; #\?
    (disp (read-char)) ; must be '='
    'debug-print)

  (define (do-vector)
    ;; read as a list!
    (read-and-anchor lvl) ; #\( is passed
    'venctor)

  (define (do-regexp)
    (disp (read-char)) ; #\/
    (let lp ((ch (read-char)))
      (cond ((eof-object? ch) (error "EOF inside regexp"))
            ((char=? #\/ ch)  (disp ch) 'regexp)
            ((char=? #\\ ch)
             (disp ch)
             (disp (read-char))
             (lp (read-char)))
            (else
             (disp ch)
             (lp (read-char))))))

  (define (do-char-set)
    (disp (read-char)) ; #\[
    (let lp ((ch (read-char)))
      (cond ((eof-object? ch) (error "EOF inside char-set"))
            ((char=? #\] ch)  (disp ch) 'char-set)
            ((char=? #\\ ch)
             (disp ch)
             (disp (read-char))
             (lp (read-char)))
            (else
             (disp ch)
             (lp (read-char))))))

  (define (do-multiline-comment)
    (disp (read-char)) ; #\|
    (let lp ((ch (read-char))
             (n 0))
      (cond ((eof-object? ch) (error "EOF inside comment"))
            ((char=? #\| ch)
             (disp ch)
             (let ((ch (read-char)))
               (cond ((char=? #\# ch)
                      (disp ch)
                      (if (= n 0)
                        'multiline-comment
                        (lp (read-char) (- n 1))))
                     (else
                      (disp ch)
                      (lp (read-char) n)))))
            ((char=? #\# ch)
             (disp ch)
             (let ((ch (read-char)))
               (cond ((eof-object? ch) (error "EOF inside comment"))
                     ((char=? #\| ch)
                      (disp ch)
                      (lp (read-char) (+ n 1)))
                     (else
                      (disp ch)
                      (lp (read-char) n)))))
            (else
             (disp ch)
             (lp (read-char) n)))))

  (define (do-string-interpolation)
    (when (not (in-string-interp))
      (in-string-interp (cons (port-name (current-input-port))
                              (port-current-line (current-input-port)))))
    (read-char) ; #\"
    (let ((str (read-string-w/o-escape (read-char) '())))
      (disp #\") ; (disp #\-)  (print "BEGIN") (display str) (print "\nEND")
      (with-input-from-string str
        (lambda ()
          (let lp ((ch (read-char)))
            (cond ((eof-object? ch) #t)
                  ((char=? #\~ ch)
                   (disp ch)
                   (cond ((char=? (peek-char) #\~)
                          (disp (read-char))
                          (lp (read-char)))
                         (else
                          (read-and-anchor lvl)
                          (lp (read-char)))))
                  ((char=? #\newline ch)
                   ;; FIXME! This gives wrong line number in case
                   ;; there are newlines in (read-and-anchor).
                   ;; We need to count number of lines in (read-and-anchor).
                   (inc! (cdr (in-string-interp)))
                   (do-newline)
                   (lp (read-char)))
                  (else
                   (disp ch)
                   (lp (read-char)))))))
      (disp #\")
      (in-string-interp #f)
      'int-string))

  (define (do-char)
    ;; this works as long as ewline of #\newline
    ;; has no manual entry, for example.
    (disp (read-char)) ; #\\
    (disp (read-char))
    'character)

  (disp #\#)
  (let ((ch (peek-char)))
    (cond ((eof-object? ch) (error "Unexpected EOF"))
          ((char=? #\? ch)  (do-debug))
          ((char=? #\( ch)  (do-vector))
          ((char=? #\/ ch)  (do-regexp))
          ((char=? #\[ ch)  (do-char-set))
          ((char=? #\| ch)  (do-multiline-comment))
          ((char=? #\" ch)  (do-string-interpolation))
          ((char=? #\\ ch)  (do-char))
          (else
           (read-char)
           (disp ch)
           'ignored))))

(define (do-bar)
  ;; we do not anchor this type of symbol.
  (disp #\|)
  (let lp ((ch (read-char)))
    (cond ((eof-object? ch) (error "EOF inside |"))
          ((char=? #\| ch)  (disp ch) 'symbol)
          ((char=? #\\ ch)
           (disp ch)
           (disp (read-char))
           (lp (read-char)))
          (else
           (disp ch)
           (lp (read-char))))))

(define (do-string)
  (let ((str (read-string-w/o-escape (read-char) '())))
    (disp #\") (disp str) (disp #\")
    'string))

(define (do-quote lvl)
  (disp #\')
  (read-w/o-anchor lvl))

(define (do-quasi-quote lvl)
  (disp #\`)
  (read-w/o-anchor lvl))

(define (do-comment)
  (disp #\;)
  (skip-until-newline)
  'comment)

(define (skip-until-newline)
  (let ((ch (read-char)))
    (cond ((eof-object? ch) ch)
          ((char=? #\newline ch)
           (do-newline))
          (else
           (disp ch)
           (skip-until-newline)))))

(define (anchor entry filename lineno)

  (define (has-source? entry) (string? (file-of entry)))

  (define (self? entry)
    (and (place-goto-anchor)
         (= lineno (line-of entry))
         (string=? filename
                   (file-of entry))))

  (cond ((self? entry)
         (display #"<a class=\"entry\" href=\"~(path-to-top (file-of entry))../goto?file=~(file-of entry)&line=~(line-of entry)\">")
         (disp (name-of entry))
         (display "</a>"))

        ((has-source? entry)
         (entry-ref-push! entry filename lineno)
         (format #t "<a class=\"entry\" href=\"~a#~a:~a\">"
                 (relative-path filename #"~(file-of entry)~(anchor-file-extension)")
                 (file-of entry)
                 (line-of entry))
         (disp (name-of entry))
         (display "</a>"))

        (else
         (disp (name-of entry)))))

(define (anchor-line)
  (let ((fl (get-file-and-lineno)))
    (format #t "<a name=\"~a:~a\"></a>"
            (car fl) (cdr fl))))

(define (has-document? sym)
  (or
   ;; we assume all exported binding has manual entry.
   (global-variable-bound? (find-module 'scmxref.used-modules)
                           sym)
   ;; or in case sym is name of a module which has manual entry
   (memq sym (documented-modules))))

(define (file->anchored-string file)
  (with-output-to-string
    (lambda ()
      (guard (e (else (get-output-string (current-output-port))))
        (read-and-anchor-file file)))))

(define (read-and-anchor-file file)
  (with-input-from-file file
    (lambda ()
      (guard (e ((is-a? e <error>)
                 ;; FIXME raise more specific condition in read-and-anchor
                 ;; or check message slot.
                 (display #"read-and-anchor: ~(~ e 'message)\n"
                          (current-error-port))
                 (copy-port (current-input-port)
                            (current-output-port))
                 (raise e))
                (else (raise e)))
        (until (eof-object? (read-and-anchor 0)))))))

(define in-string-interp (make-parameter #f))

(define (get-file-and-lineno)
  (cond ((in-string-interp) => (lambda (x) x))
        (else (cons (port-name (current-input-port))
                    (port-current-line (current-input-port))))))

(define (read-and-anchor lvl)
  (let lp ((ch (read-char)))
    (cond ((eof-object? ch) ch)

          ((char=? #\nl ch) (do-newline))
          ((char=? #\|  ch) (do-bar))
          ((char=? #\"  ch) (do-string))
          ((char=? #\;  ch) (do-comment))
          ((char=? #\'  ch) (do-quote lvl))
          ((char=? #\`  ch) (do-quasi-quote lvl))
          ((char=? #\#  ch) (do-sharp lvl))

          ;; FIXME not quite correct but it is needed
          ;; to process string interpolation
          ((char=? #\\  ch)
           (disp ch)
           (disp (read-char))
           (lp (read-char)))

          ((char=? #\)  ch)
           (when (zero? lvl)
             (disp ch)
             (error "Extra close parenthesis"))
           (disp ch)
           'close-paren)

          ((char=? #\( ch)
           (if (and (zero? lvl) (not (anchor-all-lines)))
             (anchor-line))
           (disp #\()
           (let lp ()
             (let ((x (read-and-anchor (+ lvl 1))))
               (cond ((eof-object? x) (error "unexpected EOF"))
                     ((eq? x 'close-paren) 'list)
                     (else (lp))))))

          ((char-set-contains? symbol-initial ch)
           (let* ((fl (get-file-and-lineno))
                  (filename (car fl))
                  (lineno   (cdr fl))
                  (sym      (read-symbol (peek-char) (list ch))))
             (cond ((dictionary-get sym)
                    =>
                    (lambda (entry)
                      (anchor entry filename lineno)))
                   ((has-document?  sym)
                    (display #"<a class=\"man\" href=\"~(gauche-man-base)~|sym|\">")
                    (disp sym)
                    (display "</a>"))
                   (else (disp sym))))
           'symbol)
          (else                         ; #\. #\[ #\]
           (disp ch)
           (lp (read-char))))))

(define (read-w/o-anchor lvl) ; in quote

  (define (do-unquote)
    (disp #\,)
    (if (char=? (peek-char) #\@)
      (disp (read-char)))
    (read-and-anchor lvl))

  (let lp ((ch (read-char)))
    (cond ((eof-object? ch) ch)

          ((char=? #\nl ch) (do-newline))
          ((char=? #\|  ch) (do-bar))
          ((char=? #\"  ch) (do-string))
          ((char=? #\;  ch) (do-comment))
          ((char=? #\'  ch) (do-quote lvl))
          ((char=? #\`  ch) (do-quasi-quote lvl))
          ((char=? #\#  ch) (do-sharp lvl))
          ((char=? #\,  ch) (do-unquote))

          ;; FIXME not quite correct but it is needed
          ;; to process string interpolation
          ((char=? #\\  ch)
           (disp ch)
           (disp (read-char))
           (lp (read-char)))

          ((char=? #\)  ch)
           (when (zero? lvl)
             (disp ch)
             (error "Extra close parenthesis"))
           (disp ch)
           'close-paren)

          ((char=? #\( ch)
           (if (and (zero? lvl) (not (anchor-all-lines)))
             (anchor-line))
           (disp #\()
           (let lp ()
             (let ((x (read-and-anchor (+ lvl 1))))
               (cond ((eof-object? x) (error "unexpected EOF"))
                     ((eq? x 'close-paren) 'list)
                     (else (lp))))))

          ((char-set-contains? symbol-initial ch)
           (let ((sym (read-symbol (peek-char) (list ch))))
             (disp sym)
             ch))
          (else
           (disp ch)
           (lp (read-char))))))

(provide "scmxref/read-and-anchor")
