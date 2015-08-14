;;; generic-ref-set --- Generic accessor and modifier operators.

;; Copyright © 2015  Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

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

;;; Main

(define ref
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

(define-syntax set!
  (syntax-rules ()
    ((set! <place> <expression>)
     (%set! <place> <expression>))
    ((set! <object> <field> <value>)
     (let* ((object <object>)
            (setter (lookup-setter object)))
       (setter object <field> <value>)))))

(set! (setter ref) (lambda (object field value) (set! object field value)))

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
   (list (cons bytevector? bytevector-u8-ref)
         (cons hashtable? hashtable-ref)
         (cons pair? list-ref)
         (cons string? string-ref)
         (cons vector? vector-ref))))

(define setter-table
  (alist->hashtable
   (list (cons bytevector? bytevector-u8-set!)
         (cons hashtable? hashtable-set!)
         (cons pair? list-set!)
         (cons string? string-set!)
         (cons vector? vector-set!))))

(define sparse-types
  (list hashtable?))

(define type-list
  (list boolean? bytevector? char? eof-object? hashtable? null? number? pair?
        port? procedure? string? symbol? vector?))

(define-syntax define-record-type
  (syntax-rules ()
    ((_ <name> <constructor> <pred> <field> ...)
     (begin
       (%define-record-type <name> <constructor> <pred> <field> ...)
       (push! type-list <pred>)
       (register-record-getter <pred> <field> ...)
       (register-record-setter <pred> <field> ...)))))

(define-syntax register-record-getter
  (syntax-rules ()
    ((_ <pred> (<field> <getter> . <rest>) ...)
     (let ((getters (alist->hashtable (list (cons '<field> <getter>) ...))))
       (define (getter record field)
         (let ((getter (or (ref getters field #f)
                           (error "No such field of record." record field))))
           (getter record field)))
       (hashtable-set! getter-table <pred> getter)))))

(define-syntax register-record-setter
  (syntax-rules ()
    ((_ . <rest>)
     (%register-record-setter () . <rest>))))

(define-syntax %register-record-setter
  (syntax-rules ()
    ((_ <setters> <pred> (<field> <getter>) . <rest>)
     (%register-record-setter <setters> <pred> . <rest>))
    ((_ <setters> <pred> (<field> <getter> <setter>) . <rest>)
     (%register-record-setter ((<field> <setter>) . <setters>) <pred> . <rest>))
    ((_ ((<field> <setter>) ...) <pred>)
     (let ((setters (alist->hashtable (list (cons '<field> <setter>) ...))))
       (define (setter record field value)
         (let ((setter (or (ref setters field #f)
                           (error "No such assignable field of record."
                                  record field))))
           (setter record value)))
       (hashtable-set! setter-table <pred> setter)))))

;;; generic-ref-set.body.scm ends here
