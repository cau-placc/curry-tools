%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Definitions of builtins of module Data.Global:
%

% Dynamic predicate to store temporary global entities
:- dynamic globalsStore/2.

% Initialize (if necessary) a temporary global entity identified
% by a string (first argument) corresponding to the global entity.
% The second argument is the initial expression which is used only
% if the temporary global entity is not already initialized.
% The result is a representation of the temporary global entity
% to be used by read/writeGlobalT.
'Data.Global.prim_globalT'(GName,InitExp,R) :-
        basics:string2Atom(GName,GNameA),
        (globalsStore(GNameA,Value)
         -> true
          ; initGlobalT(GNameA,InitExp)),
	!, R = 'Data.Global.GlobalT'(GName).

% Intialize temporary global entity with the ground normal form
% of the initialial expression.
initGlobalT(GNameA,InitExp) :-
        evalToken(Eval),
	user:nf(InitExp,InitVal,Eval,E1),
	user:waitUntilGround(InitVal,E1,_), % groundness required
	assert(globalsStore(GNameA,InitVal)),
        !. % silently ignore alternative values

% Read a global value:
'Data.Global.prim_readGlobalT'(Global,Val) :-
        checkGlobalEntity(Global,GlobName),
        globalsStore(GlobName,Val), !.
'Data.Global.prim_readGlobalT'(Global,_) :-
        errorGlobalUninitialized(Global).
        
% Update a global value:
'Data.Global.prim_writeGlobalT'(Global,NewVal,'Prelude.()') :-
        checkGlobalEntity(Global,GlobName),
        retract(globalsStore(GlobName,_)),
        assert(globalsStore(GlobName,NewVal)), !.
'Data.Global.prim_writeGlobalT'(Global,_,_) :-
        errorGlobalUninitialized(Global).

errorGlobalUninitialized(Global) :-
        checkGlobalEntity(Global,GlobName),
        basics:appendAtoms(['Internal error in Data.Global: global value ',
                            GlobName,' not initialized!'],ErrA),
        raise_exception(ErrA).

checkGlobalEntity('Data.Global.GlobalT'(GName),GNameA) :-
        basics:string2Atom(GName,GNameA), !.
checkGlobalEntity(Global,_) :-
        %writeLnErr(Global),
        raise_exception('Illegal global entity occurred (maybe overloaded?)').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
