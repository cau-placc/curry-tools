flatcurry
=========

This package contains libraries to deal with *FlatCurry programs*.
FlatCurry is an intermediate language to represent Curry programs
where a lot of syntactic sugar has been eliminated.
In particular, operations defined by general rules with
local declarations and pattern matching
are represented in FlatCurry by a single rule containing
case expressions and disjunctions in its right-hand side.
The precise structure of FlatCurry is defined in the
module `FlatCurry.Types`.

Note that definition of types introduced by `data` or `newtype`
declarations are still present in FlatCurry.
Although a `newtype` declaration can be considered as syntactic sugar
for a type synonym, a `newtype` declaration is important for
typing reasons, i.e., a `newtype`
is different from a type synonym. Thus, if one wants to translate
FlatCurry programs into typed target languages (e.g., Haskell),
the information about a `newtype` might be relevant.
However, if the target language is untyped, every use of a `newtype`
can be eliminated by dropping the `newtype` constructors
in the FlatCurry program. This can be done by operations
provided by the module `FlatCurry.ElimNewtype`.

Currently, this package contains the following modules:

* `FlatCurry.Compact`: This module contains operations to reduce the size
  of FlatCurry programs by combining the main module and all imports
  into a single program that contains only the operations directly or
  indirectly called from a set of main operations.
* `FlatCurry.ElimNewtype`: This module contains operations to eliminate
  definitions and uses of `newtype` in a FlatCurry program.
* `FlatCurry.Files`: This module defines operations to read and write
  FlatCurry programs.
* `FlatCurry.FlexRigid`: provides an operation to compute the rigid/flex status
  of a FlatCurry expression (typically, the right-hand side of a rule).
* `FlatCurry.Goodies`: This library provides selector operations, test and
  update operations as well as some useful auxiliary operations
  for FlatCurry data terms.
* `FlatCurry.Normalize`: This module contains operations to transform
  FlatCurry entities into some normalized form so that they can be easier
  compared for equivalence.
* `FlatCurry.Pretty`: This library provides pretty-printers for
  FlatCurry modules and all substructures (e.g., expressions).
* `FlatCurry.Read`: This library defines operations to read FlatCurry programs
  or interfaces together with all its imported modules in the current
  load path.
* `FlatCurry.Show`: This library contains operations to transform
  FlatCurry programs into string representations, either in a
  FlatCurry format or in a Curry-like syntax.
* `FlatCurry.Types`: This module defines the data types to represent
  FlatCurry programs in Curry.
* `FlatCurry.XML`: This module contains operations to convert FlatCurry
  programs into corresponding XML expressions and vice versa.
  This can be used to store Curry programs in a way independent
  of a Curry system or to use a Curry system, like PAKCS,
  as back end by other functional logic programming systems.
