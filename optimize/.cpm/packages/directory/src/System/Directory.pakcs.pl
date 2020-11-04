%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Definitions of builtins of module Directory:
%

'System.Directory.prim_doesFileExist'(FileName,Exists) :-
	string2Atom(FileName,FName),
	(existsFile(FName) -> Exists='Prelude.True' ; Exists='Prelude.False').

'System.Directory.prim_doesDirectoryExist'(DirName,Exists) :-
	string2Atom(DirName,Dir),
	(existsDirectory(Dir) -> Exists='Prelude.True' ; Exists='Prelude.False').

'System.Directory.prim_getModificationTime'(FileName,'Data.Time.CTime'(Time)) :-
	string2Atom(FileName,FName),
	fileModTime(FName,Time).

'System.Directory.prim_fileSize'(FileName,Size) :-
	string2Atom(FileName,FName),
	fileSize(FName,Size).

'System.Directory.getCurrentDirectory'(DirName) :-
	workingDirectory(Dir),
	atom2String(Dir,DirName).

'System.Directory.prim_setCurrentDirectory'(DirName,'Prelude.()') :-
	string2Atom(DirName,Dir),
	setWorkingDirectory(Dir).

'System.Directory.prim_getDirectoryContents'(DirName,EntryNames) :-
	string2Atom(DirName,Dir),
	directoryFiles(Dir,Entries),
	map2M(basics:atom2String,Entries,EntryNames).

'System.Directory.prim_createDirectory'(DirName,'Prelude.()') :-
	string2Atom(DirName,DName),
	makeDirectory(DName).

'System.Directory.prim_removeFile'(FileName,'Prelude.()') :-
	string2Atom(FileName,FName),
	deleteFile(FName).

'System.Directory.prim_removeDirectory'(DirName,'Prelude.()') :-
	string2Atom(DirName,DName),
	deleteDirectory(DName).

'System.Directory.prim_renameFile'(FileName1,FileName2,'Prelude.()') :-
	string2Atom(FileName1,FName1),
	string2Atom(FileName2,FName2),
	renameFile(FName1,FName2).

'System.Directory.prim_renameDirectory'(DirName1,DirName2,'Prelude.()') :-
	string2Atom(DirName1,DName1),
	string2Atom(DirName2,DName2),
	renameDirectory(DName1,DName2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
