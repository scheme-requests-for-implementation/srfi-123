Generic accessor and modifier operators for Scheme
==================================================

- `(ref object field)` will return the value for `field` in `object`.
- `(set! object field value)` will set `object`s `field` to `value`.

Works with vectors, strings, bytevectors, hashtables, and records.

For records, the accepted values for `field` are symbols corresponding
to the record type's field names.

`ref` also takes an optional `default` argument.  If fields in the
given object can be "empty" (e.g. a hashtable not having an
association for a given key), then the default value will be returned
when the requested field is empty.  Otherwise an error is raised.

So, assuming `hashtable` has no entry for `"foo"`:
- `(ref hashtable "foo" 'default)` => `default`
- `(ref hashtable "foo")` => error

That's all for starters.


Library usage details
---------------------

One needs to exclude `set!` and `define-record-type` when importing
`(scheme base)`, and import them from this library instead.  (One
could also import them with different names of course.)  Record types
defined with the standard `default-record-type` won't play along.  Of
course the intent is that Scheme implementations provide these
extended `set!` and `define-record-type` in their `(scheme base)` so
this problem won't apply.


SRFI-17 and SRFI-105 integration
--------------------------------

`set!` is the setter of `ref` in SRFI-17 terms, so `(set! (ref object
field) value)` works.  This is not useful on its own, but becomes
useful in combination with SRFI-105.  `ref` is also exported as
`$bracket-apply$`, so e.g. `(set! {object[field]} value)` works.  That
is still not an improvement over the normal `(set! object field
value)`, however if one also imports e.g. `:=` as a synonym for
`set!`, one can use some very "C-like" syntax: `{object[field] :=
value}`.

I strongly discourage use of that syntax in code which *otherwise*
doesn't heavily utilize SRFI-105 syntax.  The normal `(set! object
field value)` fits in much more nicely with typical Scheme code.
However, if you already use SRFI-105 syntax heavily in your code, then
the `{object[field] := value}` syntax is likely to be a welcome
addition.


Overhead
--------

Overhead while defining record types should be generally irrelevant to
an application (why would you be defining record types during a
bottleneck?); the overhead inherent to `ref` and `set!` might also
make them undesirable for the tightest of inner loops, but will surely
be negligible in the general case.

Using hashtables instead of alists might give a slight improvement.


Acknowledgments
---------------

Idea by Jorgen Schäfer AKA "forcer".
