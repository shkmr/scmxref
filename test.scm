(use gauche.test)
(use file.util) ; simplify-path
(test-start "scmxref")
(test-section "path-util")
(add-load-path ".")
(use scmxref.path-util)
(test-module 'scmxref.path-util)

(define-macro (test-relative-path from to x expect)
  `(test* ,(string-append "(relative-path \"" from "\" \""  to "\")")
          ,expect
          (relative-path ,from ,to)))

;;;
;;;   Test may fail if (current-directory) is not deep enough.
;;;
(test-relative-path ""       ""            =>  ""            )
(test-relative-path "foo"    ""            =>  ""            )
(test-relative-path "foo/"   ""            =>  "../"         )
(test-relative-path "foo/"   "foo/bar"     =>  "bar"         )
(test-relative-path "foo/"   "foo/bar/"    =>  "bar/"        )
(test-relative-path ""       "."           =>  ""            )
(test-relative-path ""       ".."          =>  "../"         )
(test-relative-path ""       "..."         => "..."          )
(test-relative-path ""       "...."        => "...."         )
(test-relative-path ""       "..."         => "..."          )
(test-relative-path ""       "./.."        =>  "../"         )
(test-relative-path ""       "../.."       =>  "../../"      )
(test-relative-path ""       ".../../"     =>  ""            )
(test-relative-path ""       "..../../"    =>  ""            )
(test-relative-path ""       ".././.."     =>  "../../"      )
(test-relative-path ""       "../../.."    =>  "../../../"   )
(test-relative-path ""       "../.../.."   =>  "../"         )

(define-macro (test-path-to-top path x expect)
  `(test* ,(string-append "(path-to-top \"" path "\"")
          ,expect
          (path-to-top ,path)))

(test-path-to-top ""             => ""            )
(test-path-to-top "foo"          => ""            )
(test-path-to-top "foo/"         => "../"         )
(test-path-to-top "foo/bar"      => "../"         )
(test-path-to-top "foo/bar/"     => "../../"      )
(test-path-to-top "foo/../bar"   => ""            )
(test-path-to-top "foo/../bar/"  => "../"         )
(test-path-to-top "foo/a../bar/" => "../../../"   )
(test-path-to-top "foo/../../"   => (test-error)  )
(test-path-to-top "../"          => (test-error)  )

(define (canonical-path-test path)
  (write path)
  (print " => " (canonical-path path)))
(print "Please eye check the behavior of canonical-path.")
(canonical-path-test "")
(canonical-path-test ".")
(canonical-path-test "..")
(canonical-path-test "...")
(canonical-path-test ".....")
(canonical-path-test "../")
(canonical-path-test "../.")
(canonical-path-test ".././")
(canonical-path-test "../../")
(canonical-path-test "../.../")
(canonical-path-test "../..../")
(canonical-path-test ".././../")
(canonical-path-test "../../../")
(canonical-path-test "../.../../")
(canonical-path-test "../..../../")
(canonical-path-test "../../../../")
(canonical-path-test "../../../../../../../../../../../../../../")

(test-section "scmxref")
(use scmxref)
(test-module 'scmxref)

;; If you don't want `gosh' to exit with nonzero status even if
;; the test fails, pass #f to :exit-on-failure.
(test-end :exit-on-failure #t)
