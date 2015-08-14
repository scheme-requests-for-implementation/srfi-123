Generic accessor and modifier operators
=======================================

Abstract
--------

Lisp dialects including Scheme have traditionally lacked short,
simple, generic syntax for accessing and modifying the fields of
arbitrary "collection" objects.  We fill this gap for Scheme by
defining generalized `ref` and `set!` operators.


Rationale
---------

In some types of code-bases, accessing and modifying fields of certain
collection objects (such as vectors, hashtables, or records) are
ubiquitous operations.  Standard Scheme APIs contain verbose procedure
names specialized for each data type, which may become very tedious to
type, and clutter the code.

In contrast, most other languages offer very short and simple syntax
for such operations, such as square bracket and dotted notation:
`object[field]` and `object.field` for access; `object[field] = value`
and `object.field = value` for modification.

To accommodate, we define a pair of generic accessor and modifier
operators that work through type-based dynamic dispatch: `(ref object
field)` for access and `(set! object field value)` for modification.

We believe the overhead involved in this is negligible in most
code-bases, and furthermore a programmer can always fall back to the
type-specific accessor and modifier procedures in performance-critical
sections of code.

The operators are specified to work on bytevectors, R6RS hashtables,
lists, strings, vectors, and all record types.  Some notes on specific
types:

- For bytevectors, 8-bit unsigned integer operations are assumed.
  There is no obvious way to incorporate other bytevector operations
  into the generalized API, and a programmer is most likely to have
  single-byte operations in mind when using a generalized API on
  bytevectors.

- For hashtables, the `ref` operator takes an additional `default`
  argument akin to `hashtable-ref`.

- Lists are supported by testing the given object for a pair.  Pairs
  themselves are senseless to support because `(set! pair car value)`
  contains the same number of words as `(set-car! pair value)`.  In
  the `ref` equivalent, it contains one word more: `(ref pair car)`.

- For records, the accepted values for the `field` parameter are
  symbols corresponding to the record type's field names.  The
  overhead involved in looking up the correct accessor of modifier
  falls under the same rationale as other kinds of overhead involved
  with this SRFI.

Alists are unfortunately impossible to support due to the lack of a
reliable `alist?` predicate.  (It's ambiguous in that every alist is
also a list, and any list may coincidentally have the structure of an
alist.)

A `ref*` procedure taking an arbitrary number of `field` arguments and
walking through several collections was considered, but deemed
sub-optimal because it doesn't play well with collections that may
have "empty" fields, and usually one doesn't walk through deep
structures at once, and instead binds intermediate results to a
variable.  Nevertheless, it is trivial to define if desired:

    (define (ref* object field . fields)
      (if (null? fields)
          (ref object field)
          (apply ref* (ref object field) fields)

This might be a better candidate for SRFI-105's `$bracket-apply$` than
regular `ref`.


Integration with SRFI-17 and SRFI-105
-------------------------------------

The `set!` operator in this SRFI does not conflict with the one in
SRFI-17.  The reference implementation extends the SRFI-17 `set!` and
thus supports the functionality of both SRFI-17 and the one described
here.

Additionally, if SRFI-17 is supported, the `ref` procedure's "setter"
may be defined as: `(lambda (object field value) (set! object field
value))`.  This is uninteresting in its own right, but can yield an
interesting combination with SRFI-105.  In code that already uses
SRFI-105 heavily, a programmer may define `$bracket-apply$` as a
synonym to `ref`, define `:=` as a synonym to `set!`, and then use the
following syntax: `{object[field] := value}`.


Specification
-------------

- `(ref object field)` (procedure)
- `(ref object field default)`

Returns the value for `field` in `object`.  If `object` is of a type
whose fields can be "empty" or "unassigned" (e.g. a hashtable), then
the value of `default` is returned if given, and otherwise an error
raised.

Valid types for `object` are: bytevectors, hashtables, pairs, strings,
vectors, and all record types.

Valid types for `field` depend on the type of `object`.  For
bytevectors, hashtables, strings, and vectors, refer to their
respective `*-ref` procedures.  For pairs, refer to `list-ref`.  For
records, symbols that correspond with the record type's field names
are taken.

If SRFI-17 is supported, then the `ref` procedure has the following
setter: `(lambda (object field value) (set! object field value))`

- `(set! object field value)` (syntax)

Sets the value for `field` in `object` to `value`.

Valid types for `object` and `field` are the same as in the `ref`
procedure.

Note: This operator is only a syntax keyword because it overloads the
normal `set!` syntax.


Considerations when using as a library
--------------------------------------

The intent of this SRFI is to encourage Scheme systems to extend the
semantics of their default `set!` operator in line with this SRFI.  On
the meanwhile, it can be used as a library, but certain considerations
apply.

The `set!` and `define-record-type` exports of the library conflict
with the ones in `(scheme base)`, so either have to be renamed, or
more typically, the ones from `(scheme base)` excluded.

Record types not defined with the `default-record-type` exported by
this library won't work with `ref` and `set!`.


Implementation
--------------

A reference implementation as a library is found in the version
control repository of this SRFI.

It might be desirable for Scheme systems to offer a more efficient
`type-of` procedure than the one used in this implementation, which in
the worst case consumes linear time with regard to the number of types
(including every record type) within the system, albeit with a very
small constant factor: one call to each type predicate.


Acknowledgments
---------------

Original idea and some input during design by Jorgen Schäfer.
