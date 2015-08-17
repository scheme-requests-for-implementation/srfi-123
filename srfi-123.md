Generic accessor and modifier operators
=======================================

Author
------

Taylan Ulrich Bayırlı/Kammer, taylanbayirli at Google Mail


Status
------

This SRFI is currently in <em>draft</em> status.  Here is
[an explanation](http://srfi.schemers.org/srfi-process.html) of each
status that a SRFI can hold.  To provide input on this SRFI, please
send email to <code><a href="mailto:srfi minus 123 at srfi dot
schemers dot org">srfi-123@<span
class="antispam">nospam</span>srfi.schemers.org</a></code>.  To
subscribe to the list, follow
[these instructions](http://srfi.schemers.org/srfi-list-subscribe.html).
You can access previous messages via the mailing list
[archive](http://srfi-email.schemers.org/srfi-123).

  - Received: 2015/8/14
  - Draft #1 published: 2015/8/15
  - Draft #2 published: 2015/8/16


Abstract
--------

Lisp dialects including Scheme have traditionally lacked short,
simple, generic syntax for accessing and modifying the fields of
arbitrary "collection" objects.  We fill this gap for Scheme by
defining generalized accessors, and an associated SRFI-17 setter.


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

To accommodate, we define a pair of generic accessor operators that
work through type-based dynamic dispatch: `(ref object field)`, and
`(ref* object field1 field2 ...)` for chained access.

We define `~` as a synonym to `ref*`, and define an SRFI-17 setter for
it: `(set! (~ object field1 field2 ...) value)`.

Plain `ref`, instead of allowing chaining, takes an optional `default`
argument for objects such as hashtables: `(ref table key default)`.

We believe the overhead involved in the dynamic dispatch is negligible
in most cases, and furthermore a programmer can always fall back to
type-specific accessor and modifier procedures in performance-critical
sections of code.

The operators are specified to work on bytevectors, R6RS hashtables,
lists/pairs, strings, vectors, non-opaque record types, and SRFI-4
vectors if present.  (R6RS and SRFI-99 can produce opaque record
types; SRFI-9 and R7RS cannot.)  Some notes on specific types:

- For bytevectors, 8-bit unsigned integer operations are assumed.
  There is no obvious way to incorporate other bytevector operations
  into the generalized API, and a programmer is most likely to have
  single-byte operations in mind when using a generalized API on
  bytevectors.

    ```
    (define bv (bytevector 0 1 2 3))
    (ref bv 2)  ;=> 2
    (set! (~ bv 2) 5)
    (ref bv 2)  ;=> 5
    ```

- For hashtables, the `ref` operator takes an optional `default`
  argument whose semantics is akin to `hashtable-ref`.  (This is not
  possible with `ref*`; it will always behave as when no default
  argument is passed.)

    ```
    (define table (make-eqv-hashtable))
    (ref table "foo" 'not-found)  ;=> not-found
    (set! (~ table "foo") "Foobar.")
    (ref table "foo")  ;=> "Foobar."
    (ref table "bar")  ;error: Object has no entry for field.
    ```

- When a pair is encountered, the field argument may be the symbols
  `car` or `cdr`, or an integer index indicating the pair should be
  viewed as the head of a list.

    ````
    (ref '(a b c . d) cdr)  ;=> (b c . d)
    (ref '(a b c . d) 2)  ;=> c
    ````

- For records, the accepted values for the `field` parameter are
  symbols corresponding to the record type's field names.  The
  overhead involved in looking up the correct accessor of modifier
  falls under the same rationale as other kinds of overhead involved
  with this SRFI.

    ```
    (define-record-type <foo> (make-foo a b) foo?
      (a foo-a set-foo-a!)
      (b foo-b))
    (define foo (make-foo 0 1))
    (ref foo 'a)  ;=> 0
    (set! (~ foo 'b) 2)  ;error: No such assignable field of record.
    ```

Alists are difficult to support due to the lack of a reliable `alist?`
predicate.  (It's ambiguous in that every alist is also a list, and
any list may coincidentally have the structure of an alist.)  It was
considered to support non-integer keyed alists as a special case, but
this would lead to silent code breakage when a programmer forgot about
the API inconsistency and exchanged a non-integer key for an integer
key in existing code.  It was also considered to drop list support in
favor of alist support, but that idea discarded as well because the
hypothetical `alist-set!` is an exceedingly rare operation.
(Prepending an entry to the front, possibly hiding another entry with
the same key, is more common.)


Integration with SRFI-105
-------------------------------------

The `ref*` procedure is a good candidate for SRFI-105's
`$bracket-apply$`.  Indeed the reference implementation exports
`$bracket-apply$` as a synonym to `ref*`.  In code that already uses
SRFI-105 heavily, a programmer may additionally define `:=` as a
synonym to `set!`, and then use the following syntax:
`{object[field] := value}`.

    #!curly-infix
    (import (rename (only (scheme base) set!) (set! :=)))
    (define vec (vector 0 1 2 3))
    {vec[1] + vec[2]}  ;=> 3
    {vec[2] := 4}
    {vec[1] + vec[2]}  ;=> 5

The square brackets accept a chain of fields, since they have the
semantics of `ref*`: `{matrix[i j]}`.


Specification
-------------

- `(ref object field)` (procedure)
- `(ref object field default)`

Returns the value for `field` in `object`.  An error is raised if
`object` has no field identified by `field`.  (This error will often
come from the underlying accessor procedure.)

    (ref #(0 1 2) 3)  ;error: vector-ref: Index out of bounds.

If `object` is of a "sparse" type, meaning its fields can be "empty"
or "unassigned" (e.g. a hashtable), and the requested field is empty,
then the value of `default` is returned if given, and otherwise an
error raised.

    (ref hashtable unassigned-key 'default)  ;=> default
    (ref hashtable unassigned-key)  ;error

If `object` is not of a sparse type, then passing `default` is an
error.

    (ref '(0 1 2) 3 'default)  ;error: list-ref: Too many arguments.
                               ;Unless the implementation's list-ref
                               ;does something else.

Valid types for `object` are: bytevectors, hashtables, pairs, strings,
vectors, non-opaque record types, and SRFI-4 vectors if present.  Only
hashtables are a sparse type.  Implementations are encouraged to
expand this list of types with any further types they support.

Valid types for `field` depend on the type of `object`.  For
bytevectors, hashtables, strings, vectors, and SRFI-4 vectors, refer
to their respective `*-ref` procedures.  For pairs, refer to
`list-ref`.  For records, symbols that correspond with the record
type's field names are allowed.

- `(ref* object field field* ...)` (procedure)
- `(~ object field field* ...)`

The semantics is of this procedure is as follows:

    (ref* object field)            = (ref object field)
    (ref* object field field+ ...) = (ref* (ref object field) field+ ...)

It has an associated SRFI-17 setter, which does the expected thing:

    (set! (~ obj f1 f2 f3) value)

changes the value that would be returned from `(~ obj f1 f2 f3)` to
`value`.  Note that this procedure can be accessed as `(setter ref*)`
when needed:

    (define (store-item! field-chain value)
      (apply (setter ref*) the-store (append field-chain (list value))))

- `(register-getter-with-setter! type getter sparse?)` (procedure)

Registers a new type/getter/setter triple for the dynamic dispatch.
`Type` is a type predicate, `getter` is a procedure that has a setter
associated with it, and `sparse?` is a Boolean indicating whether the
type is a sparse type (see `ref` specification).


Considerations when using as a library
--------------------------------------

The intent of this SRFI is to encourage Scheme systems to extend their
standard library in accordance with the above specification.  On the
meanwhile, the reference implementation can be used as a separate
library, but certain considerations apply.

The `define-record-type` export of the library conflicts with the one
in `(scheme base)`, so either has to be renamed, or more typically,
the one from `(scheme base)` excluded.

Record types not defined with the `define-record-type` exported by
this library won't work with `ref` and `ref*`.

The `define-record-type` exported by this library expands to a record
type definition followed with a command, essentially eliminating the
"definition" status of `define-record-type`.  This means, for example,
that you can't use it more than once (only at the end) within the
internal definitions sequence of a body.  It's a rare use-case, but if
you need it, you can nest each additional `define-record-type` use in
a further `(let () ...)`.  It works fine in the top-level, since there
definitions and commands can be interspersed.


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

Thanks to Jorgen Schäfer for inspiring me to write this SRFI and
making the initial suggestion for the `ref` procedure and ternary
`set!` syntax.

The `ref*` procedure with its `~` synonym and SRFI-17 setter (which
replaced the initially considered ternary `set!` syntax) seems to have
first appeared in Gauche.  Thanks to Shiro Kawai:
<http://blog.practical-scheme.net/gauche/20100428-shorter-names>


Copyright and license
---------------------

Copyright (C) Taylan Ulrich Bayırlı/Kammer (2015). All Rights Reserved.

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
