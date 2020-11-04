%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Definitions of builtins of module Distribution
%

'Language.Curry.Distribution.curryCompiler'(CS) :- atom2String(pakcs,CS).

'Language.Curry.Distribution.curryCompilerMajorVersion'(V) :-
        compilerMajorVersion(V).

'Language.Curry.Distribution.curryCompilerMinorVersion'(V) :-
        compilerMinorVersion(V).

'Language.Curry.Distribution.curryCompilerRevisionVersion'(V) :-
        compilerRevisionVersion(V).

'Language.Curry.Distribution.curryRuntime'(PrologS) :-
        prolog(Prolog), atom2String(Prolog,PrologS).

'Language.Curry.Distribution.curryRuntimeMajorVersion'(V) :-
        prologMajorVersion(V).

'Language.Curry.Distribution.curryRuntimeMinorVersion'(V) :-
        prologMinorVersion(V).

'Language.Curry.Distribution.baseVersion'(BVS) :-
        baseVersion(BVA), atom2String(BVA,BVS).

'Language.Curry.Distribution.installDir'(PHS) :-
	installDir(PH)
	 -> atom2String(PH,PHS)
	  ; raise_exception('Language.Curry.Distribution.installDir: cannot determine installation directory!').
