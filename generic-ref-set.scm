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

(import
 (rename (scheme base)
         (set! _set!)
         (define-record-type _define-record-type))
 (scheme case-lambda)
 (r6rs hashtables)
 (srfi 1))

;;; Helpers

(define-syntax push!
  (syntax-rules ()
    ((_ <list-var> <x>)
     (set! <list-var> (cons <x> <list-var>)))))

(define-syntax alist-add!
  (syntax-rules ()
    ((_ <alist-var> <key> <value>)
     (push! <alist-var> (cons <key> <value>)))))

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
    ((set! <var> <val>)
     (_set! <var> <val>))
    ((set! <object> <field> <value>)
     (let* ((object <object>)
            (setter (lookup-setter object)))
       (setter object <field> <value>)))))

(define (lookup-getter object)
  (let ((entry (assv (type-of object) getter-table)))
    (if entry
        (cdr entry)
        (error "No generic getter for object's type." object))))

(define (lookup-setter object)
  (let ((entry (assv (type-of object) setter-table)))
    (if entry
        (cdr entry)
        (error "No generic setter for object's type." object))))

(define (sparse-type? object)
  (memv (type-of object) sparse-types))

(define (type-of object)
  (find (lambda (pred) (pred object)) type-list))

(define getter-table
  (list (cons bytevector? bytevector-u8-ref)
        (cons string? string-ref)
        (cons vector? vector-ref)
        (cons hashtable? hashtable-ref)))

(define setter-table
  (list (cons bytevector? bytevector-u8-set!)
        (cons string? string-set!)
        (cons vector? vector-set!)
        (cons hashtable? hashtable-set!)))

(define sparse-types
  (list hashtable?))

(define type-list
  (list boolean? bytevector? char? eof-object? hashtable? null? number? pair?
        port? procedure? string? symbol? vector?))

(define-syntax define-record-type
  (syntax-rules ()
    ((_ <name> <constructor> <pred> <field> ...)
     (begin
       (_define-record-type <name> <constructor> <pred> <field> ...)
       (push! type-list <pred>)
       (register-record-getter <pred> <field> ...)
       (register-record-setter <pred> <field> ...)))))

(define-syntax register-record-getter
  (syntax-rules ()
    ((_ <pred> (<field> <getter> . <rest>) ...)
     (let ((getters (list (cons '<field> <getter>) ...)))
       (define (getter record field)
         (let ((entry (assv field getters)))
           (if entry
               ((cdr entry) record)
               (error "No such field of record." record field))))
       (alist-add! getter-table <pred> getter)))))

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
     (let ((setters (list (cons '<field> <setter>) ...)))
       (define (setter record field value)
         (let ((entry (assv field setters)))
           (if entry
               ((cdr entry) record value)
               (error "No such assignable field of record." record field))))
       (alist-add! setter-table <pred> setter)))))
