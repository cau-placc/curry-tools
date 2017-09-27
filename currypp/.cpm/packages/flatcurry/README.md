flatcurry
=========

This package contains libraries to deal with FlatCurry programs.
Currently, it contains the following modules:

* `FlatCurry.Compact`: This module contains operations to reduce the size
  of FlatCurry programs by combining the main module and all imports
  into a single program that contains only the functions directly or
  indirectly called from a set of main functions.
* `FlatCurry.Files`: This module defines operations to read and write
  FlatCurry programs.
* `FlatCurry.FlexRigid`: provides a function to compute the rigid/flex status
  of a FlatCurry expression (right-hand side of a function definition).
* `FlatCurry.Goodies`: This library provides selector functions, test and
  update operations as well as some useful auxiliary functions
  for FlatCurry data terms.
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
