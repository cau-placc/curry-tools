%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Prolog implementation of builtins of module ReadShowTerm:
%

'ReadShowTerm.prim_showQTerm'(Term,String) :-
        readShowTerm:prim_showQTerm(Term,String).

'ReadShowTerm.prim_showTerm'(Term,String) :-
        readShowTerm:prim_showTerm(Term,String).

'ReadShowTerm.prim_readsQTerm'(String,Term) :-
        readShowTerm:prim_readsQTerm(String,Term).

'ReadShowTerm.prim_readsUnqualifiedTerm'(Prefixes,String,Term) :-
        readShowTerm:prim_readsUnqualifiedTerm(Prefixes,String,Term).
