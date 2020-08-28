%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Prolog implementation of builtins of module Network.Socket:
%

% create a server side socket bound to a port number.
'Network.Socket.prim_listenOn'(Port,Socket) :- listenOnNewSocket(Port,_,Socket).


% create a server side socket with a fresh port.
'Network.Socket.prim_listenOnFresh'('Prelude.(,)'(Port,Socket)) :-
	listenOnNewSocket(Port,_,Socket).


% return the first connection to a socket as a read/write stream:
'Network.Socket.prim_socketAccept'(Socket,
    'Prelude.(,)'(ClientS,'$stream'('$inoutstream'(InStream,OutStream)))) :-
        socketAccept(Socket,Client,InStream,OutStream),
	atom2String(Client,ClientS), !.

% return a connection to a socket within a time limit as a read/write stream,
% otherwise Nothing:
'Network.Socket.prim_waitForSocketAccept'(Socket,TimeOut,Result) :-
	(waitForSocketClientStream(Socket,TimeOut,Client,InStream,OutStream)
	 -> atom2String(Client,ClientS),
	    Result = 'Prelude.Just'('Prelude.(,)'(ClientS,
			      '$stream'('$inoutstream'(InStream,OutStream))))
	  ; Result = 'Prelude.Nothing').


% Closes a server socket.
'Network.Socket.prim_sClose'(Socket,'Prelude.()') :- socketClose(Socket).


% open a connection to a Unix socket:
'Network.Socket.prim_connectToSocket'(SHst,SNr,
                            '$stream'('$inoutstream'(InStream,OutStream))) :-
        string2Atom(SHst,Host), !,
        connect2socket(Host,SNr,InStream,OutStream).


