Curry Tools
===========

This directory contains some base tools for Curry
that are used by different Curry systems, like PAKCS or KiCS2.

Currently, it contains the following tools:

`cpm`
This repository contains the Curry package manager (CPM)
with the main executable `cypm`.
CPM is a tool to distribute and install Curry packages.
It can be used to download further Curry libraries and applications.

`cpns`:
This directory contains scripts and executables for the
Curry Port Name Server (CPNS) which is responsible for managing the
symbolic names assigned to the named sockes and ports on a local machine
(see library `NamedSocket` for more details).

`optimize`:
The implementation of optimization tools cor Curry,
in particular, a transformation tool to replace Boolean equalities
by unification constraints.

`www`:
This directory contains scripts and executables for the
event handler server that is required to execute dynamic web
pages implemented with the libraries HTML or WUI.
