%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Prolog implementation of builtins of module System.IOExts:
%

:- dynamic globalAssoc/2.

'System.IOExts.prim_setAssoc'(Key,Val,'Prelude.()') :-
        string2Atom(Key,KeyA),
	(retract(globalAssoc(KeyA,_)) -> true ; true),
	assertz(globalAssoc(KeyA,Val)),
	!.

'System.IOExts.prim_getAssoc'(Key,R) :-
        string2Atom(Key,KeyA),
	(globalAssoc(KeyA,Val) -> R='Prelude.Just'(Val) ; R='Prelude.Nothing'),
	!.

% shell command execution:
'System.IOExts.prim_execCmd'(CmdString,'Prelude.(,,)'(StdIn,StdOut,StdErr)) :-
	string2Atom(CmdString,Cmd),
	execCommand(Cmd,StdIn,StdOut,StdErr).


% shell command execution:
'System.IOExts.prim_connectToCmd'(CmdString,
                                  '$stream'('$inoutstream'(StdOut,StdIn))) :-
	string2Atom(CmdString,Cmd),
	execCommand(Cmd,StdIn,StdOut,std).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
