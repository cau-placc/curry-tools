%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Prolog implementation of builtins of module System.IOExts:
%

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
