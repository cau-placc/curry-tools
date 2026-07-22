Curry Tools
===========

This directory contains some base tools for Curry
that are used by different Curry systems, like PAKCS or KiCS2.
These are:

`cpm`
This directory contains the Curry package manager (CPM)
with the main executable `cypm`.
CPM is a tool to distribute and install Curry packages.
It can be used to download further Curry libraries and applications.

`optimize`:
The implementation of optimization tools for Curry,
in particular, a transformation tool to replace Boolean equalities
by unification constraints.


Important note:
---------------

If these tools are used in a Curry system, it is assumed
that this directory is installed as directory `currytools`
in the main installation directory of the Curry system.
Otherwise, some links will be broken after installation
of the tools.
