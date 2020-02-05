%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Definitions of builtins of module Debug.Profile:
%

% return statistics about the PAKCS process:
'Debug.Profile.getProcessInfos'(Infos) :-
	(getCurrentGCs(GCs)
         -> I1=['Prelude.(,)'('Debug.Profile.GarbageCollections',GCs)] ; I1=[]),
	(getCurrentChoiceSize(Choice)
         -> I2=['Prelude.(,)'('Debug.Profile.Choices',Choice)|I1] ; I2=I1),
	(getCurrentHeapSize(Heap)
         -> I3=['Prelude.(,)'('Debug.Profile.Heap',Heap)|I2] ; I3=I2),
	(getCurrentStackSize(Stack)
         -> I4=['Prelude.(,)'('Debug.Profile.Stack',Stack)|I3] ; I4=I3),
	(getCurrentCodeSize(Code)
         -> I5=['Prelude.(,)'('Debug.Profile.Code',Code)|I4] ; I5=I4),
	(getCurrentMemorySize(Mem)
         -> I6=['Prelude.(,)'('Debug.Profile.Memory',Mem)|I5] ; I6=I5),
	(getElapsedTime(ETime)
         -> I7=['Prelude.(,)'('Debug.Profile.ElapsedTime',ETime)|I6] ; I7=I6),
	(getRunTime(RTime)
         -> I8=['Prelude.(,)'('Debug.Profile.RunTime',RTime)|I7] ; I8=I7),
	Infos = I8.

% turn on garbage collector:
'Debug.Profile.garbageCollectorOn'('Prelude.()') :- garbageCollectorOn.

% turn off garbage collector:
'Debug.Profile.garbageCollectorOff'('Prelude.()') :- garbageCollectorOff.

% turn off garbage collector:
'Debug.Profile.garbageCollect'('Prelude.()') :- garbageCollect.

