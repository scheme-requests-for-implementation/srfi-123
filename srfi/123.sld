(define-library (srfi 123)
  (export
   ref set! define-record-type (rename ref $bracket-apply$))
  (import
   (rename (except (scheme base) set!)
           (define-record-type %define-record-type))
   (scheme case-lambda)
   (r6rs hashtables)
   (srfi 1)
   (rename (srfi 17) (set! %set!)))
  (include "123.body.scm"))
