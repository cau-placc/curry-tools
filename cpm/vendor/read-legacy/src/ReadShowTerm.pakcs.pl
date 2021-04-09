%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Prolog implementation of builtins of module ReadShowTerm:
%

'ReadShowTerm.prim_showTerm'(Term,String) :-
        readShowTerm:prim_showTerm(Term,String).

'ReadShowTerm.prim_readsUnqualifiedTerm'(Prefixes,String,Term) :-
        readShowTerm:prim_readsUnqualifiedTerm(Prefixes,String,Term).
