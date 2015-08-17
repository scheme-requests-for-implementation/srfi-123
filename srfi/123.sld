(define-library (srfi 123)
  (export
   ref ref* ~ register-getter-with-setter!
   $bracket-apply$
   define-record-type
   set! setter getter-with-setter)
  (import
   (rename (except (scheme base) set!)
           (define-record-type %define-record-type))
   (scheme case-lambda)
   (r6rs hashtables)
   (srfi 1)
   (srfi 17)
   (srfi 31))
  (cond-expand
   ((library (srfi 4))
    (import (srfi 4)))
   (else))
  (include "123.body.scm"))
