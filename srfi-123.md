% Generic accessor and modifier operators

Author
------

Taylan Ulrich Bayırlı/Kammer, taylanbayirli at Google Mail


Status
------

This SRFI is currently in *final* status.  Here is
[an explanation](http://srfi.schemers.org/srfi-process.html) of each
status that a SRFI can hold.

To provide input on this SRFI, please
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
  - Draft #3 published: 2015/8/17
  - Draft #4 published: 2015/8/18
  - Draft #5 published: 2015/8/23 (code changes only)
  - Draft #6 published: 2015/8/24
  - Draft #7 published: 2015/8/26
  - Draft #8 published: 2015/9/5
  - Draft #9 published: 2015/9/7
  - Finalized: 2015/10/4


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

    (ref #(a b c) 1)  ;=> b
    (ref* #(a (x y #u8(1 2 3)) c) 1 2 0)  ;=> 1

We define `~` as a synonym to `ref*`, and define a SRFI-17 setter for
it.

    (define struct #(a (x y #u8(1 2 3)) c))
    (set! (~ struct 1 2 0) 4)
    struct  ;=> #(a (x y #u8(4 2 3)) c)

Plain `ref`, instead of allowing chaining, takes an optional "default"
argument for objects such as hashtables.

    (define table (make-eqv-hashtable))
    (ref table "foo" 'not-found)  ;=> not-found
    (set! (~ table "foo") "Foobar.")
    (ref table "foo" 'not-found)  ;=> "Foobar."

Lack of a default argument is an error in this case.  Since `ref*`
cannot take default arguments for any fields it accesses, it is an
error when a hashtable key in the chain is not found.

    (define table (make-eqv-hashtable))
    (define lst (list 0 1 table 3))
    (ref* lst 2 "foo" 'x)  ;error while accessing "foo" from table

We believe the overhead involved in the dynamic dispatch is negligible
in most cases, and furthermore a programmer can always fall back to
type-specific accessor and modifier procedures in performance-critical
sections of code.

The operators are specified to work on bytevectors, R6RS hashtables,
lists/pairs, strings, vectors, non-opaque record types, SRFI-4
vectors, and SRFI-111 boxes.  (R6RS and SRFI-99 can produce opaque
record types; SRFI-9 and R7RS cannot.)  Some notes on specific types:

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

- However, some implementations provide SRFI-4 vectors by tagging
  bytevectors, such that SRFI-4 vectors are not disjoint types from
  bytevectors.  In that case, the SRFI-4 type of the bytevector
  dictates the semantics.

    ```
    (define bv (s16vector 0 1 2 3))
    (bytevector? bv)  ;=> #t
    (bytevector-u8-ref bv 2)  ;=> (result depends on endianness)
    (ref bv 2)  ;=> 2
    ```

- When a pair is encountered, the field argument may be the symbols
  `car` or `cdr`, or an integer index indicating the pair should be
  viewed as the head of a list.

    ````
    (ref '(a b c . d) 'cdr)  ;=> (b c . d)
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

- For boxes, the symbol `*` is used to indicate the one value field of
  the box.  This is mainly useful for `ref*`:

    ```
    (define struct (list 0 (vector (box (cons 'a 'b)))))
    (ref* struct 1 0 '* 'cdr)
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
-------------------------

The `ref*` procedure is a good candidate for SRFI-105's
`$bracket-apply$`.  Indeed the sample implementation exports
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

Within this section, whenever a situation is described as being an
error, a Scheme implementation supporting error signaling should
signal an error.

- `(ref object field)` (procedure)
- `(ref object field default)`

Returns the value for `field` in `object`.  It is an error if `object`
has no field identified by `field`.

    (ref #(0 1 2) 3)  ;error: vector-ref: Index out of bounds.

If `object` is of a "sparse" type, meaning its fields can be "empty"
or "unassigned" (e.g. a hashtable), and the requested field is empty,
then the value of `default` is returned.  It is an error if the
`default` argument is not provided in this case.

    (ref hashtable unassigned-key 'default)  ;=> default
    (ref hashtable unassigned-key)  ;error

If `object` is not of a sparse type, then providing the `default`
argument is an error.

    (ref '(0 1 2) 3 'default)  ;error: list-ref: Too many arguments.

Valid types for `object` are: bytevectors, hashtables, pairs, strings,
vectors, non-opaque record types, SRFI-4 vectors, and SRFI-111 boxes.
Only hashtables are a sparse type.  Implementations are encouraged to
expand this list of types with any further types they support.

Valid types for `field` depend on the type of `object`.  For
bytevectors, hashtables, strings, vectors, and SRFI-4 vectors, refer
to their respective `*-ref` procedures.  For pairs, the symbols `car`
and `cdr` are accepted, as well as non-negative integers as with
`list-ref`.  For records, symbols that correspond with the record
type's field names are allowed.  For boxes, the symbol `*` is used to
denote the one value field of the box.

A conforming implementation must be prepared for SRFI-4 vector types
and bytevectors not being disjoint types, and treat SRFI-4 vectors
suitably and not as regular bytevectors.

A conforming implementation must also be prepared for boxes being a
non-opaque record type instead of a disjoint type, and treat them
correctly despite that fact.

The `ref` procedure has an associated SRFI-17 setter, although the one
of `ref*` is strictly more powerful.

    (define vec (vector 0 1 2))
    (set! (ref vec 0) 3)
    vec  ;=> #(3 1 2)

- `(ref* object field field* ...)` (procedure)
- `(~ object field field* ...)`

The semantics of this procedure is as follows:

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

The getter will be called with two arguments: the object whose field
should be accessed, and an object identifying the field to be
accessed.  The setter will be called with one additional argument
which is the value to be assigned to the given field of the given
object.

**Warning:** This procedure is strictly meant for when defining a new
disjoint type which isn't already handled by `ref`.  In practice, this
means it should only be used with newly defined opaque record types,
or types defined with some implementation-specific method which,
unlike `define-record-type`, doesn't automatically register a getter
and setter for the type.  If any two type predicates registered with
the system both return true for any Scheme object, the behavior is
undefined.  (A custom getter or setter may, however, dispatch to
different actions based on some property of the given object, based on
the `field` argument, or based on anything else.)

It is conceivable that this method will become deprecated after a
system has been invented which ties together the definition of a new
opaque record type with the definitions of its getter and setter.
This is considered outside the scope of this SRFI.


Considerations when using as a library
--------------------------------------

The intent of this SRFI is to encourage Scheme systems to extend their
standard library in accordance with the above specification.  On the
meanwhile, the sample implementation can be used as a separate
library, but certain considerations apply.

The `define-record-type` export of the library conflicts with the one
in `(scheme base)`, so either has to be renamed, or more typically,
the one from `(scheme base)` excluded.

Record types not defined with the `define-record-type` exported by
this library won't work with `ref`, `ref*`, or their setters.

This problem does not apply to implementations supporting inspection
of records and record types.


Implementation
--------------

A sample implementation as a library is found in the version control
repository of this SRFI.

It might be desirable for Scheme systems to offer a more efficient
`type-of` procedure than the one used in this implementation, which in
the worst case consumes linear time with regard to the number of types
(including every record type) within the system, albeit with a very
small constant factor: one call to each type predicate.


Acknowledgments
---------------

Thanks to Jorgen Schäfer for inspiring me to write this SRFI and
making the initial suggestion for the `ref` procedure and ternary
`set!` syntax, as well as providing continuous input.

The `ref*` procedure with its `~` synonym and SRFI-17 setter (which
replaced the initially considered ternary `set!` syntax) seems to have
first appeared in Gauche.  Thanks to Shiro Kawai and Issac Trotts:
<http://blog.practical-scheme.net/gauche/20100428-shorter-names>

Thanks to Evan Hanson for the idea of using a throw-away `define` in
the expansion of `define-record-type` so as not to disturb a sequence
of internal definitions.

Thanks to Vincent St-Amour, Eli Barzilay, and others in the Racket IRC
channel for raising my awareness against action-at-a-distance bugs
that might result from abuse of the imperative
`register-getter-with-setter!`.

Thanks also to everyone else on the discussion mailing list for their
input.


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
