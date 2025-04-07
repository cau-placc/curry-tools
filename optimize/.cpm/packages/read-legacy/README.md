read-legacy
===========

This package contains the library `ReadShowTerm` which provides
operations to convert ground terms to strings and vice versa.
These operations work similarly to the standard `show` and `read`
operations but are implemented by generic external operations
so that they are more efficient.

Due to the generic implementation of these operations, the
`read...` operations must be called with a list of module names
containing all constructores occurring in the string representation
of the data term to be read.
