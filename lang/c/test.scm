(use gauche.test)
(use gauche.parameter)
(use text.tree)
(test-start "lang.c.c89-scan")
(add-load-path "../..")
(use lang.c.c89-scan)
(test-module 'lang.c.c89-scan)

(define (scan str)
  (with-input-from-string str
    (lambda ()
      (port-map (lambda (x)
                  (cons (token-type x)
                        (token-string x)))
                c89-scan))))

(define (test-scan str expect)
  (test* str expect (scan str)))

(test-section "CARM 2.3 tokens")
(test-scan "forwhile;" '((IDENTIFIER . "forwhile") (SEMICOLON . ";")))
(test-scan "b>x;"      '((IDENTIFIER . "b") (> . ">") (IDENTIFIER . "x") (SEMICOLON . ";")))
(test-scan "b->x;"     '((IDENTIFIER . "b") (PTR_OP . "->") (IDENTIFIER . "x") (SEMICOLON . ";")))
(test-scan "b--x;"     '((IDENTIFIER . "b") (DEC_OP . "--") (IDENTIFIER . "x") (SEMICOLON . ";")))
(test-scan "b---x;"    '((IDENTIFIER . "b") (DEC_OP . "--") (- . "-") (IDENTIFIER . "x") (SEMICOLON . ";")))

(test-section "integer constants")
(test-scan "1234;"    '((integer . "1234"  ) (SEMICOLON . ";")))
(test-scan "012;"     '((integer . "012"   ) (SEMICOLON . ";")))
(test-scan "0x12;"    '((integer . "0x12"  ) (SEMICOLON . ";")))

(test-section "character constants")
(test-scan "'a';"     '((character . "'a'"    ) (SEMICOLON . ";")))
(test-scan "'A';"     '((character . "'A'"    ) (SEMICOLON . ";")))
(test-scan "' ';"     '((character . "' '"    ) (SEMICOLON . ";")))
(test-scan "'?';"     '((character . "'?'"    ) (SEMICOLON . ";")))
(test-scan "'\\r';"   '((character . "'\\r'"  ) (SEMICOLON . ";")))
(test-scan "'\\0';"   '((character . "'\\0'"  ) (SEMICOLON . ";")))
(test-scan "'\"';"    '((character . "'\"'"   ) (SEMICOLON . ";")))
(test-scan "'\\377';" '((character . "'\\377'") (SEMICOLON . ";")))
(test-scan "'%';"     '((character . "'%'"    ) (SEMICOLON . ";")))
(test-scan "'\\23';"  '((character . "'\\23'" ) (SEMICOLON . ";")))
(test-scan "'8';"     '((character . "'8'"    ) (SEMICOLON . ";")))
(test-scan "'\\\\';"  '((character . "'\\\\'" ) (SEMICOLON . ";")))
(test-scan "'ABCD';"  '((character . "'ABCD'" ) (SEMICOLON . ";")))

(test-section "floating point constants")
(test-scan "0.;"       '((double . "0."     )       (SEMICOLON . ";")))
(test-scan "3e1;"      '((double . "3e1"    )       (SEMICOLON . ";")))
(test-scan "3.14159;"  '((double . "3.14159")       (SEMICOLON . ";")))
(test-scan ".0;"       '((double . ".0"     )       (SEMICOLON . ";")))
(test-scan "1.0E-3;"   '((double . "1.0E-3" )       (SEMICOLON . ";")))
(test-scan "1e-3;"     '((double . "1e-3"   )       (SEMICOLON . ";")))
(test-scan "1.0;"      '((double . "1.0"    )       (SEMICOLON . ";")))
(test-scan "0.00034;"  '((double . "0.00034")       (SEMICOLON . ";")))
(test-scan "2e+9;"     '((double . "2e+9"   )       (SEMICOLON . ";")))
(test-scan "1.0f;"     '((float  . "1.0f"   )       (SEMICOLON . ";")))
(test-scan "1.0e67L;"  '((long-double . "1.0e67L")  (SEMICOLON . ";")))
(test-scan "1.37E+6L;" '((long-double . "1.37E+6L") (SEMICOLON . ";")))
(test-scan "0E1L;"     '((long-double . "0E1L"   )  (SEMICOLON . ";")))
(test-scan "0x1.0p1;"  '((double . "0x1.0p1")       (SEMICOLON . ";")))
(test-scan "0x1.0;"    (test-error <scan-error>)) ; "hexadecimal floating constant requires an exponent"

(test-section "string constants")
(test-scan "\"\";"          '((string . "\"\"")       (SEMICOLON . ";")))
(test-scan "\"\\\"\";"      '((string . "\"\\\"\"")   (SEMICOLON . ";")))
(test-scan "\"Copyright 2000 \\\nTexas Instruments. \""
           '((string . "\"Copyright 2000 \\\nTexas Instruments. \"")))

(test-section "other")
(test-scan "X++Y;"     '((IDENTIFIER . "X") (INC_OP . "++") (IDENTIFIER . "Y")  (SEMICOLON . ";")))
(test-scan "-12ul;"    '((-  . "-") (integer . "12ul") (SEMICOLON . ";")))
(test-scan "x**2;"     '((IDENTIFIER . "x") (* . "*") (* . "*") (integer . "2") (SEMICOLON . ";")))
(test-scan "A*=B;"     '((IDENTIFIER . "A") (MUL_ASSIGN . "*=") (IDENTIFIER . "B")  (SEMICOLON . ";")))

;; ## operator is processed by cpp, lexer is not expected to see it.
;;(test-scan "while##DO;" '())

(test-section "token-string")

(define p? (make-parameter #f))

(define (test-copy file)
  (let ((x (with-input-from-file file
             (lambda ()
               (port-map (lambda (x)
                           (if (p?) (print-token x))
                           (token-string x))
                         c89-scan)))))
    (with-output-to-file "fo.scm" (cut write-tree x))
    (sys-system #"diff ~|file| fo.scm")))

(for-each (lambda (f)
            (test* f 0 (test-copy f)))
          '("c/hello.c"))

(test-end :exit-on-failure #t)
