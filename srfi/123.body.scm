;;; generic-ref-set --- Generic accessor and modifier operators.

;; Copyright © 2015  Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>

;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Helpers

(define-syntax push!
  (syntax-rules ()
    ((_ <list-var> <x>)
     (set! <list-var> (cons <x> <list-var>)))))

(define (alist->hashtable alist)
  (let ((table (make-eqv-hashtable 100)))
    (for-each (lambda (entry)
                (hashtable-set! table (car entry) (cdr entry)))
              alist)
    table))

(define (pair-ref pair key)
  (cond
   ((eqv? car key)
    (car pair))
   ((eqv? cdr key)
    (cdr pair))
   (else
    (list-ref pair key))))

(define (pair-set! pair key value)
  (cond
   ((eqv? car key)
    (set-car! pair value))
   ((eqv? cdr key)
    (set-cdr! pair value))
   (else
    (list-set! pair key value))))

;;; SRFI-4 support

(cond-expand
 ((library (srfi 4))
  (define srfi-4-getters
    (list (cons s8vector? s8vector-ref)
          (cons u8vector? u8vector-ref)
          (cons s16vector? s16vector-ref)
          (cons u16vector? u16vector-ref)
          (cons s32vector? s32vector-ref)
          (cons u32vector? u32vector-ref)
          (cons s64vector? s64vector-ref)
          (cons u64vector? u64vector-ref)))
  (define srfi-4-setters
    (list (cons s8vector? s8vector-set!)
          (cons u8vector? u8vector-set!)
          (cons s16vector? s16vector-set!)
          (cons u16vector? u16vector-set!)
          (cons s32vector? s32vector-set!)
          (cons u32vector? u32vector-set!)
          (cons s64vector? s64vector-set!)
          (cons u64vector? u64vector-set!)))
  (define srfi-4-types
    (list s8vector? u8vector? s16vector? u16vector? s32vector? u32vector?
          s64vector? u64vector?)))
 (else
  (define srfi-4-getters '())
  (define srfi-4-setters '())
  (define srfi-4-types '())))

;;; Main

(define %ref
  (case-lambda
    ((object field)
     (let ((getter (lookup-getter object))
           (sparse? (sparse-type? object)))
       (if sparse?
           (let* ((not-found (cons #f #f))
                  (result (getter object field not-found)))
             (if (eqv? result not-found)
                 (error "Object has no entry for field." object field)
                 result))
           (getter object field))))
    ((object field default)
     (let ((getter (lookup-getter object)))
       (getter object field default)))))

(define (%ref* object field . fields)
  (if (null? fields)
      (%ref object field)
      (apply %ref* (%ref object field) fields)))

(define (%set! object field value)
  (let ((setter (lookup-setter object)))
    (setter object field value)))

(define ref
  (getter-with-setter
   %ref
   (lambda (object field value)
     (%set! object field value))))

(define ref*
  (getter-with-setter
   %ref*
   (rec (set!* object field rest0 . rest)
     (if (null? rest)
         (%set! object field rest0)
         (apply set!* (ref object field) rest0 rest)))))

(define ~ ref*)

(define $bracket-apply$ ref*)

(define (lookup-getter object)
  (or (hashtable-ref getter-table (type-of object) #f)
      (error "No generic getter for object's type." object)))

(define (lookup-setter object)
  (or (hashtable-ref setter-table (type-of object) #f)
      (error "No generic setter for object's type." object)))

(define (sparse-type? object)
  (memv (type-of object) sparse-types))

(define (type-of object)
  (find (lambda (pred) (pred object)) type-list))

(define getter-table
  (alist->hashtable
   (append
    (list (cons bytevector? bytevector-u8-ref)
          (cons hashtable? hashtable-ref)
          (cons pair? pair-ref)
          (cons string? string-ref)
          (cons vector? vector-ref))
    srfi-4-getters)))

(define setter-table
  (alist->hashtable
   (append
    (list (cons bytevector? bytevector-u8-set!)
          (cons hashtable? hashtable-set!)
          (cons pair? pair-set!)
          (cons string? string-set!)
          (cons vector? vector-set!))
    srfi-4-setters)))

(define sparse-types
  (list hashtable?))

(define type-list
  (append
   (list boolean? bytevector? char? eof-object? hashtable? null? number? pair?
         port? procedure? string? symbol? vector?)
   srfi-4-types))

(define (register-getter-with-setter! type getter sparse?)
  (push! type-list type)
  (set! (~ getter-table type) getter)
  (set! (~ setter-table type) (setter getter))
  (when sparse?
    (push! sparse-types type)))

(define-syntax define-record-type
  (syntax-rules ()
    ((_ <name> <constructor> <pred> <field> ...)
     (begin
       (%define-record-type <name> <constructor> <pred> <field> ...)
       (register-getter-with-setter!
        <pred>
        (getter-with-setter (record-getter <field> ...)
                            (record-setter <field> ...))
        #f)))))

(define-syntax record-getter
  (syntax-rules ()
    ((_ (<field> <getter> . <rest>) ...)
     (let ((getters (alist->hashtable (list (cons '<field> <getter>) ...))))
       (lambda (record field)
         (let ((getter (or (ref getters field #f)
                           (error "No such field of record." record field))))
           (getter record)))))))

(define-syntax record-setter
  (syntax-rules ()
    ((_ . <rest>)
     (%record-setter () . <rest>))))

(define-syntax %record-setter
  (syntax-rules ()
    ((_ <setters> (<field> <getter>) . <rest>)
     (%record-setter <setters> . <rest>))
    ((_ <setters> (<field> <getter> <setter>) . <rest>)
     (%record-setter ((<field> <setter>) . <setters>) . <rest>))
    ((_ ((<field> <setter>) ...))
     (let ((setters (alist->hashtable (list (cons '<field> <setter>) ...))))
       (lambda (record field value)
         (let ((setter (or (ref setters field #f)
                           (error "No such assignable field of record."
                                  record field))))
           (setter record value)))))))

;;; generic-ref-set.body.scm ends here
