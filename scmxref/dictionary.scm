(select-module scmxref)
;;;
;;;  DICTIONARY
;;;
(define documented-modules
  (make-parameter
   ;;
   ;;  This is list of module names that has manual entry.
   ;;  If you do not want to too much link to manual entry,
   ;;  or find anything missing, customize the this list.
   ;;  Especially if you do (documented-modules '())
   ;;  you will not get any link to manual entries
   ;;  except for R5RS syntactic keywords.
   ;;
   '(
     crypt.bcrypt
     dbm
     file.util
     gauche.array
     gauche.charconv
     gauche.cgen
     gauche.collection
     gauche.config
     gauche.configure
     gauche.dictionary
     gauche.fcntl
     gauche.generator
     gauche.hook
     gauche.interactive
     gauche.lazy
     gauche.listener
     gauche.logger
     gauche.mop.propagate
     gauche.mop.singleton
     gauche.mop.validator
     gauche.net
     gauche.parameter
     gauche.parseopt
     gauche.partcont
     gauche.process
     gauche.record
     gauche.regexp                      ; autoloaded
     gauche.reload
     gauche.selector
     gauche.sequence
     gauche.syslog
     gauche.termios
     gauche.test
     gauche.threads
     gauche.time
     gauche.uvector
     gauche.version
     gauche.vport
     srfi-1                    ; List library
                                        ;srfi-2  -- and-let* is in core syntax
     srfi-4                             ; Homogeneous vectors
     srfi-11                            ; Let-values
     srfi-13                            ; String library
     srfi-14                            ; Character-set library
     srfi-19                         ; Time data types and procedures
     srfi-27                         ; Sources of Random Bits
     srfi-29                         ; Localization
     srfi-37                         ; args-fold
     srfi-42                         ; Eager comprehensions
     srfi-43                         ; Vecotor library
     srfi-55                         ; Requiring extensions
     srfi-60                         ; Integers as bits
     srfi-98                         ; Accessing environment variables
     srfi-106                        ; Basic socket interface
     binary.io
     binary.pack
     compat.norational
     control.job
     control.thread-pool
     crypt.bcrypt
     data.random
     dbi
     dbm
     dbm.fsdbm
     dbm.gdbm
     dbm.ndbm
     dbm.odbm
     file.filter
     file.util
     math.const
     math.mt-random
     math.prime
     os.windows
     rfc.822
     rfc.base64
     rfb.cookie
     rfc.ftp
     rfc.hmac
     rfc.http
     rfc.ip
     rfc.icmp
     rfc.json
     rfc.md5
     rfc.mime
     rfc.quoted-printable
     rfc.sha
     rfc.uri
     rfc.zlib
     sxml.ssax
     sxml.sxpath
     sxml.tools
     sxml.serializer
     text.csv
     text.diff
     text.gettext
     text.html-lite
     text.parse
     text.progress
     text.sql
     text.tr
     text.tree
     text.unicode
     util.combinations
     util.digest
     util.isomorph
     util.lcs
                                        ; util.list -- obsoleted
     util.match
     util.queue
     util.rbtree
     util.record
     util.relation
     util.sparse
     util.stream
     util.trie
     util.toposort
     www.cgi
     www.cgi.test
     )))

(define *dictionary* #f)

(define (dictionary-get name)
  (hash-table-get *dictionary* name #f))

(define (dictionary-put! name type file line)
  (hash-table-put! *dictionary*
                   name (make <entry> :name name :type type :file file :line line)))

(define-class <entry> ()
  ((name :init-value #f
         :init-keyword :name
         :accessor name-of)
   (type :init-value #f
         :init-keyword :type
         :accessor type-of)
   (file :init-value #f
         :init-keyword :file
         :accessor file-of)
   (line :init-value 0
         :init-keyword :line
         :accessor line-of)
   (ref  :init-value '()
         :accessor ref-of)
   ))

(define (entry-ref-push! entry file line)
  (unless (member (list file line) (ref-of entry))
    (push! (ref-of entry) (list file line))))

(define (dictionary-file-list)
  (let ((result '()))
    (hash-table-for-each *dictionary*
                         (lambda (name entry)
                           (if (and (file-of entry)
                                    (not (member (file-of entry) result)))
                               (push! result (file-of entry)))))
    result))

(define (dictionary-definition-list file)
  (let ((result '()))
    (hash-table-for-each *dictionary*
                         (lambda (name entry)
                           (if (and (file-of entry)
                                    (string=? file (file-of entry)))
                               (push! result entry))))
    (sort result (lambda (x y)
                   (< (line-of x)
                      (line-of y))))))

;;;
;;;  BUILD-DICTIONARY
;;;
(define-module scmxref.used-modules)

(define (read-source-file file)
  (guard (e (else #f))
    (with-input-from-file file read-source)))

(define (read-source)

  (define (definition? exp)
    (and (pair? exp)
         (symbol? (car exp))
         (string-prefix-ci? "define" (symbol->string (car exp)))))

  (define (definition-name exp)
    (let lp ((second (cadr exp)))
      (cond ((pair? second) (lp (car second)))
            ((symbol? second) second)
            (else
             (display #"Warning: strange definition:(~(car exp) ~|second| ...\n"
                      (current-error-port))))))

  (define (definition-type exp) (car exp))

  (define (putit! exp)
    (let* ((name (definition-name exp))
           (type (definition-type exp))
           (info (debug-source-info exp))
           (file (list-ref info 0))
           (line (list-ref info 1)))

      (cond ((not (symbol? name)) #t)
            ((dictionary-get name)
             =>
             (lambda (entry)
               (cond ((and (string=? file (file-of entry))
                           (= line line (line-of entry)))
                      'we-already-have-it)
                     ((and (eq? type 'define-method)
                           (eq? (type-of entry) 'define-method))
                      ;; XXX
                      (dictionary-put! name type file line))
                     (else
                      (display #"Duplicate definition of ~|name| as ~|type| found at ~|file|:~|line|. \
                          \nPrevisous definition: (~(type-of entry) ~(name-of entry) ...) \
                             at ~(file-of entry):~(line-of entry).\n"
                               (current-error-port))
                      (dictionary-put! name type file line)))))
            (else
             (dictionary-put! name type file line)))

      (cond ((eq? type 'define-module)
             (for-each (lambda (exp)
                         (if (use-documented-module? exp) (useit! exp)))
                       exp)))
      ))

  (define (use-documented-module? exp)
    (and (pair? exp)
         (eq? (car exp) 'use)
         (memq (cadr exp) (documented-modules))))

  (define (useit! exp)
    (eval exp (find-module 'scmxref.used-modules)))

  (guard (e ((is-a? e <error>)
             (display #"read-source: ~(~ e 'message)\n"
                      (current-error-port)))
            (else #f))
    (port-for-each
     (lambda (exp)
       (cond ((definition?            exp) (putit! exp))
             ((use-documented-module? exp) (useit! exp))))
     read)))

(define (build-dictionary files)
  (if (null? (documented-modules))
    (eval '(extend null) (find-module 'scmxref.used-modules))
    (eval '(extend user) (find-module 'scmxref.used-modules)))
  (set! *dictionary* (make-hash-table))
  (for-each read-source-file files))

(provide "scmxref/dictionary")
