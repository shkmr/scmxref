(define-module scmxref
  (use gauche.parameter)
  (use scmxref.path-util)
  (use srfi-13)
  (use text.html-lite)
  (use text.tree)
  (require "scmxref/dictionary")
  (require "scmxref/read-and-anchor")
  (export documented-modules
          build-dictionary
          read-source-file
          dictionary-get
          dictionary-file-list
          dictionary-definition-list
          anchor-all-lines
          anchor-file-extension
          gauche-man-base
          place-goto-anchor
          read-and-anchor
          read-and-anchor-file
          file->anchored-string
          display-referer
          display-index-table
          display-index.html
          html-of
          file-of
          line-of
          stylesheet
          ))
(select-module scmxref)

;;
(define-constant stylesheet "\
table.index {
    width: 100%;
}
th.index {
    text-align: left;
    vertical-align: top;
    width: 20%;
}

a {
  text-decoration: none;
}
a.menu {
  color: navy;
}
a.file {
  color: mediumblue;
}
a.remove {
  color: red;
}
a.entry {
  color: royalblue;
}
a.ref {
  color: green;
}
a.lineno {
  color: dimgray;
}
a.man {
  color: seagreen;
}
")

;;;
;;; fscm has to be one of members of (dictionary-file-list)
;;;
(define (html-of fscm) (string-append fscm (anchor-file-extension)))

(define (display-referer entry)
  (define (sort-ref ref)
    (sort ref
          (lambda (x y)
            (if (string=? (list-ref x 0)
                          (list-ref y 0))
              (< (list-ref x 1)
                 (list-ref y 1))
              (string<? (list-ref x 0)
                        (list-ref y 0))))))
  (for-each (lambda (ref)
              (format #t "  <a class=\"ref\" href=\"~a#~a:~a\">~a:~a</a>  "
                      (string-append (list-ref ref 0)
                                     (anchor-file-extension))
                      (list-ref ref 0)
                      (list-ref ref 1)
                      (list-ref ref 0)
                      (list-ref ref 1)))
            (sort-ref (ref-of entry))))
  
(define (display-index-table fscm)
  (display "<table class=\"index\">\n")
  (for-each (lambda (entry)
              (format #t "<tr><th class=\"index\"><a class=\"entry\" href=\"~a#~a:~a\">"
                      (html-of fscm)
                      fscm
                      (line-of entry))
              (disp (name-of entry))
              (display "</a></th><td>")
              (if (anchor-all-lines)
                (display-referer entry))
              (display "</td></tr>\n"))
            (dictionary-definition-list fscm))
    (display "</table>\n"))

(define (display-index.html)
  (write-tree
   (html:html
    (html:head (html:title "scmxref")
               (html:style stylesheet))
    (html:body
     (html:h1 "Index")
     (map (lambda (fscm)
              (let ((html (html-of fscm)))
                (list (html:h2 (html:a :href html fscm))
                      (with-output-to-string
                        (lambda ()
                          (display-index-table fscm))))))
          (sort (dictionary-file-list)))))))

(provide "scmxref")
