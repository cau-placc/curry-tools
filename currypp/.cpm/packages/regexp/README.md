regexp: Regular expression matching
===================================

This package contains libraries dealing with regular expressions:

* `RegExpSem`: This library specifies the semantics of
  regular expressions and regular expression matching in a high-level
  manner via non-deterministic operations. It might be useful
  as an oracle to test more efficient regular expression matchers.
* `RegExp`:  This library defines a structure of regular expressions
  and a simple match operation for regular expression.
  This library is used by the Curry preprocessor to translate
  integrated code in the form of POSIX extended regular expressions
  into Curry programs.

--------------------------------------------------------------------------
