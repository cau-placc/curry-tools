wl-pprint
=========

This package provides pretty printing combinators for Curry.
It uses the interface of Daan Leijen's library for Haskell
(http://www.cs.uu.nl/~daan/download/pprint/pprint.html).
The linear-time bounded implementation is based on an approach by Olaf Chitil
(http://www.cs.kent.ac.uk/pubs/2006/2381/index.html).
Note that the implementation of `fill` and `fillBreak` is not linear-time bounded
Besides well-known pretty printing combinators, this library also supports ANSI
escape codes for formatting and colorisation of documents in text terminals
(see https://en.wikipedia.org/wiki/ANSI_escape_code).
