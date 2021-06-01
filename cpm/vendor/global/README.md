global: Handling global entities in programs
============================================

This package contains a library `Data.Global`
to handle global entities in Curry programs.
A global entity has a name declared in the program.
Its value can be accessed and modified by IO actions.

Global entities can be declared as persistent so that
their values are stored across different program executions
or temporary so that they will be stored only in memory.

Currently, it is still experimental so that its interface might
be slightly changed in the future.

Usage
-----

A temporary global entity `gt` is a top-level constant of type
`GlobalT t`. If `v` is an initial value `v` of type `t`,
where the type `t` does not contain type variables or type class
contraints, the temporary global entity should be declared in
a module `Mod` by:

    gt :: GlobalT t
    gt = globalT "Mod.gt" v

The first argument is the qualified name of this program entity
and used as a unique name for this global value.

Similarly, a persistent global entity `gp` with an initial value `v`
of type `t` could be declared by:

    gt :: GlobalP t
    gt = globalPersistent f v

where the type `t` must not contain type variables and support
`Read` and `Show` instances. `f` is the file name
where the global value is persistently stored
(the file is created and initialized with `v` if it does not exist).

--------------------------------------------------------------------------
