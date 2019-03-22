------------------------------------------------------------------------------
--- Library to support network programming with sockets.
--- In standard applications, the server side uses the operations
--- `listenOn` and `socketAccept` to provide some service
--- on a socket, and the client side uses the operation
--- `connectToSocket` to request a service.
---
--- @author Michael Hanus
--- @version December 2018
------------------------------------------------------------------------------

module Network.Socket
  (Socket, listenOn, listenOnFresh,
   accept, waitForSocketAccept, close, connectToSocket)
 where

import System.IO (Handle)

--- The abstract type of sockets.
external data Socket

---------------------------------------------------------------------
-- Server side operations:

--- Creates a server side socket bound to a given port number.
listenOn :: Int -> IO Socket
listenOn port = prim_listenOn $# port

prim_listenOn :: Int -> IO Socket
prim_listenOn external

--- Creates a server side socket bound to a free port.
--- The port number and the socket is returned.
listenOnFresh :: IO (Int,Socket)
listenOnFresh external


--- Returns a connection of a client to a socket.
--- The connection is returned as a pair consisting of a string identifying
--- the client (the format of this string is implementation-dependent)
--- and a handle to a stream communication with the client.
--- The handle is both readable and writable.
accept :: Socket -> IO (String,Handle)
accept s = prim_socketAccept $## s

prim_socketAccept :: Socket -> IO (String,Handle)
prim_socketAccept external


--- Waits until a connection of a client to a socket is available.
--- If no connection is available within the time limit, it returns Nothing,
--- otherwise the connection is returned as a pair consisting
--- of a string identifying the client
--- (the format of this string is implementation-dependent)
--- and a handle to a stream communication with the client.
--- @param socket - a socket
--- @param timeout - milliseconds to wait for input (< 0 : no time out)
waitForSocketAccept :: Socket -> Int -> IO (Maybe (String,Handle))
waitForSocketAccept s timeout = (prim_waitForSocketAccept $## s) $# timeout

prim_waitForSocketAccept :: Socket -> Int -> IO (Maybe (String,Handle))
prim_waitForSocketAccept external


--- Closes a server socket.
close :: Socket -> IO ()
close s = prim_sClose $## s

prim_sClose :: Socket -> IO ()
prim_sClose external


---------------------------------------------------------------------
-- Client side operations:

--- Creates a new connection to a Unix socket.
--- @param host - the host name of the connection
--- @param port - the port number of the connection
--- @return the handle of the stream (connected to the port port@host)
---         which is both readable and writable
connectToSocket :: String -> Int -> IO Handle
connectToSocket host port = (prim_connectToSocket $## host) $# port

prim_connectToSocket :: String -> Int -> IO Handle
prim_connectToSocket external

---------------------------------------------------------------------
