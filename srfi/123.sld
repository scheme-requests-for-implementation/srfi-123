(define-library (srfi 123)
  (export
   ref ref* ~ $bracket-apply$ set! define-record-type)
  (import
   (rename (except (scheme base) set!)
           (define-record-type %define-record-type))
   (scheme case-lambda)
   (r6rs hashtables)
   (srfi 1)
   (srfi 31)
   (rename (srfi 17) (set! %set!)))
  (cond-expand
   ((library (srfi 4))
    (import (srfi 4)))
   (else))
  (include "123.body.scm"))
