(define-module scmxref.path-util
  (use file.util)
  (use srfi-11) ; let-values
  (export canonical-path
          relative-path
          path-to-top
          ))
(select-module scmxref.path-util)
;;
;; ---  not really belongs to scmxref
;;

;;;
;;;  Pathname utilities: canonical-path, relative-path, path-to-top.
;;;

;;
;;  Function: <code>canonical-path path</code>
;;
;;    Calculate canonical path of <code>path</code>.
;;
(define (canonical-path path)
  (simplify-path
   (sys-normalize-pathname
    path :absolute #t :canonicalize #t)))

;;
;; Function: <code>path-to-top path</code>
;;
;;   Calculate path to top directory from dirname of <code>path</code>.
;;   An error is raised if <code>path</code> goes beyond top.
;;
;;   Example:
;;
;;     (path-to-top "")            => ""
;;     (path-to-top "foo")         => ""
;;     (path-to-top "foo/")        => "../"
;;     (path-to-top "foo/bar")     => "../"
;;     (path-to-top "foo/bar/")    => "../../"
;;     (path-to-top "foo/../bar")  => ""
;;     (path-to-top "foo/../bar/") => "../"
;;     (path-to-top "foo/../../")  => ERROR
;;     (path-to-top "../")         => ERROR
;;
(define (path-to-top pathname)
  (let ((path (simplify-path
               (if (absolute-path? pathname)
                 (string-copy pathname 1 (string-length pathname))
                 pathname))))
    (let lp ((path path)
             (lis  '()))
      (if (eq? (string-scan path "../") 0)
        (error "Invalid path " pathname)
        (cond ((string=? path "/")
               (apply string-append lis))
              ((string-scan path #\/ 'after)
               => (lambda (path)
                    (lp path (cons "../" lis))))
              (else
               (apply string-append lis)))))))

;;
;; Function: <code>relative-path from to</code>
;;
;;   Calculate relative path from <code>from</code> to <code>to</code>.
;;
;;   Example: in case (current-directory) => "/home/smith"
;;
;;     (relative-path "" "")                => ""
;;     (relative-path "foo" "")             => ""
;;     (relative-path "foo/" "")            => "../"
;;     (relative-path "foo/" "foo/bar")     => "bar"
;;     (relative-path "foo/" "foo/bar/")    => "bar/"
;;     (relative-path "../foo/" "foo/bar/") => "../smith/foo/bar/"
;;     (relative-path "" "../..")           => "../../"
;;     (relative-path "" "../../..")        => "../../"  ; parent of root is itself.
;;
;;   BUG: the last case does not work as expected.
;;
(define (relative-path from to)
  (let lp ((f0 (canonical-path from))
           (t0 (canonical-path to)))
    (let-values (((f1 f2) (string-scan f0 #\/ 'both))
                 ((t1 t2) (string-scan t0 #\/ 'both)))
      (cond ((and f1 t1 (string=? f1 t1))
             (lp f2 t2))
            (else
             (string-append (path-to-top f0) t0))))))

(provide "scmxref/path-util")
