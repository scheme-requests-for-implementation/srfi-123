(define-library (generic-ref-set)
  (export
   ref set! define-record-type)
  (import
   (rename (except (scheme base) set!)
           (define-record-type %define-record-type))
   (scheme case-lambda)
   (r6rs hashtables)
   (srfi 1)
   (rename (srfi 17) (set! %set!)))
  (include "generic-ref-set.body.scm"))
