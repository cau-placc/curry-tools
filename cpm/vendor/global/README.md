global: Handling global entities in programs
============================================

This package contains a library to handle global entities.
A global entity has a name declared in the program.
Its value can be accessed and modified by IO actions.
Furthermore, global entities can be declared as persistent so that
their values are stored across different program executions.

Currently, it is still experimental so that its interface might
be slightly changed in the future.

A global entity `g` with an initial value `v`
of type `t` must be declared by:

    g :: Global t
    g = global v spec

Here, the type `t` must not contain type variables and
`spec` specifies the storage mechanism for the
global entity (see type `GlobalSpec` in the library).

--------------------------------------------------------------------------
