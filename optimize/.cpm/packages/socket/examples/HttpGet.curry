-- A simple example showing the direct connection to Unix sockets
-- by using the `Network.Socket` library.

import System.IO

import Network.Socket(connectToSocket)

-- An I/O action that shows the answer of a web server to the
-- request of a document:
httpGet :: String -> String -> IO ()
httpGet host doc = do
 str <- connectToSocket host 80
 hPutStr str ("GET " ++ doc ++ " HTTP/1.0\n\n")
 hFlush str
 showStreamContents str

-- Show the complete contents of an output stream:
showStreamContents :: Handle -> IO ()
showStreamContents str = do
 b <- hIsEOF str
 if b then return ()
      else do l <- hGetLine str
              putStrLn l
              showStreamContents str

-- A test:
main :: IO ()
main = httpGet "www.google.com" "/index.html"

