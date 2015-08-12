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

That's all.


Details
-------

One needs to exclude `set!` and `define-record-type` when importing
`(scheme base)`, and import them from this library instead.  (One
could also import them with different names of course.)  Record types
defined with the standard `default-record-type` won't play along.  Of
course the intent is that Scheme implementations provide these
extended `set!` and `define-record-type` in their `(scheme base)` so
this problem won't apply.

Overhead while defining record types should be generally irrelevant to
an application (why would you be defining record types during a
bottleneck?); the overhead inherent to `ref` and `set!` might also
make them undesirable for the tightest of inner loops, but will surely
be negligible in the general case.

Using hashtables instead of alists might give a slight improvement.


Acknowledgments
---------------

Idea by Jorgen Schäfer AKA "forcer".
