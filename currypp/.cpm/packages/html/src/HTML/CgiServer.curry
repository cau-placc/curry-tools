------------------------------------------------------------------------------
--- This library contains the implementation of the server based on CGI
--- to implement dynamic web pages.
---
--- @author Michael Hanus
--- @version October 2017
--- @category web
------------------------------------------------------------------------------

{-# OPTIONS_CYMAKE -Wno-incomplete-patterns #-}

module HTML.CgiServer
 ( runFormServerWithKey, runFormServerWithKeyAndFormParams
 , intForm, intFormMain
 ) where

import Char        ( isSpace )
import Directory    (getHomeDirectory)
import Distribution (installDir)
import HTML.Base
import HtmlCgi
import IO
import List ( intercalate )
import NamedSocket
import Profile
import Random       ( getRandomSeed, nextInt )
import ReadNumeric  ( readNat )
import ReadShowTerm ( showQTerm, readsQTerm )
import System
import Time
--import Unsafe(showAnyQExpression) -- to show status of cgi server

------------------------------------------------------------------------------
--- The server implementing an HTML form (possibly containing input fields).
--- It receives a message containing the environment of the client's
--- web browser, translates the HTML form w.r.t. this environment
--- into a string representation of the complete HTML document
--- and sends the string representation back to the client's browser
--- by binding the corresponding message argument.
--- @param url - the URL of this executable.
--- @param cgikey - a unique key to identify this CGI script (used for safe
---                 storing of event handlers in this server)
--- @param hformact - an IO action returning an HTML form
runFormServerWithKey :: String -> String -> IO HtmlForm -> IO ()
runFormServerWithKey url cgikey hformact =
  runFormServerWithKeyAndFormParams url cgikey [] hformact

--- The server implementing an HTML form (possibly containing input fields).
--- It receives a message containing the environment of the client's
--- web browser, translates the HTML form w.r.t. this environment
--- into a string representation of the complete HTML document
--- and sends the string representation back to the client's browser
--- by binding the corresponding message argument.
--- @param url - the URL of this executable.
--- @param cgikey - a unique key to identify this CGI script (used for safe
---                 storing of event handlers on the web server)
--- @param formparams - form parameters added to the initial and all
---                     subsequent forms
--- @param hformact - an IO action returning an HTML form
runFormServerWithKeyAndFormParams :: String -> String -> [FormParam]
                                  -> IO HtmlForm -> IO ()
runFormServerWithKeyAndFormParams url cgikey formparams hformact = do
  args <- getArgs
  let (timeout,rargs) = stripTimeoutArg args
  case rargs of
    ["-port",port,"-scriptkey",skey] -> startCgiServer timeout port skey
    _ -> putErrLn $ "ERROR: cgi server called with illegal arguments"
 where
  stripTimeoutArg args = case args of
    ("-servertimeout":tos:rargs) ->
         (tryReadNat defaultCgiServerTimeout tos, rargs)
    _ -> (defaultCgiServerTimeout,args)

  startCgiServer timeout port scriptkey = do
    time  <- getClockTime
    ltime <- toCalendarTime time
    (state,htmlstring) <- computeFormInStateAndEnv url cgikey formparams
                             (initialServerState time) scriptkey hformact []
    putStr htmlstring
    hClose stdout
    if isServerStateWithoutHandlers state
     then done
     else -- start server process:
      do let portname = port++scriptkey
         socket <- listenOn portname
         putErrLn $ calendarTimeToString ltime ++
                    ": server started on port " ++ portname
         registerCgiServer url portname
         serveCgiMessagesForForm timeout url cgikey portname formparams
                                 hformact socket state

-- The default timeout period for the cgi server in milliseconds:
defaultCgiServerTimeout :: Int
defaultCgiServerTimeout = 7200000 -- two hours


-- The main server loop:
serveCgiMessagesForForm :: Int -> String -> String -> String -> [FormParam]
                        -> IO HtmlForm -> Socket -> ServerState -> IO ()
serveCgiMessagesForForm servertimeout url cgikey portname
                        fparams initform socket = serveCgiMessages
 where
  serveCgiMessages state =
    if isServerStateWithoutHandlers state
    then do -- terminate server due to inactivity
            ltime <- getLocalTime
            putErrLn $ calendarTimeToString ltime ++
                       ": terminated due to empty handler list"
            unregisterCgiServer portname
            sClose socket
    else waitForSocketAccept socket servertimeout >>=
         maybe (do -- terminate server due to inactivity
                   ltime <- getLocalTime
                   putErrLn $ calendarTimeToString ltime ++
                              ": terminated due to timeout"
                   unregisterCgiServer portname
                   sClose socket )
               (\ (rhost,hdl) -> do
                  hostname <- getHostname
                  if rhost `elem` ["localhost","localhost.localdomain",hostname]
                     || take 8 rhost == "127.0.0."
                   then readCgiServerMsg hdl >>=
                        maybe (hClose hdl >> serveCgiMessages state)
                              (serveCgiMessage state hdl)
                   else putErrLn ("Ignored message from: "++rhost) >>
                        hClose hdl >> serveCgiMessages state )

  -- Process the received CgiServerMsg:
  serveCgiMessage _ hdl StopCgiServer = do
    hClose hdl
    ltime <- getLocalTime
    putErrLn $ calendarTimeToString ltime ++
              ": server terminated by stop message"
    unregisterCgiServer portname
    sClose socket

  serveCgiMessage state hdl CleanServer = do
    hClose hdl
    nstate <- cleanOldEventHandlers state
    serveCgiMessages nstate

  serveCgiMessage oldstate hdl GetLoad = do
    state <- cleanOldEventHandlers oldstate
    serverload <- getServerLoad state
    hPutStrLn hdl serverload
    hClose hdl
    serveCgiMessages state

  serveCgiMessage oldstate hdl SketchStatus = do
    state <- cleanOldEventHandlers oldstate
    serverstatus <- getServerStatus state
    hPutStrLn hdl serverstatus
    hClose hdl
    serveCgiMessages state

  serveCgiMessage state hdl SketchHandlers =
    reportStatus state hdl sketchEventHandler
   where
    sketchEventHandler (key,time,_,_,gkey) = do
      ltime <- toCalendarTime time
      return $ "No. " ++ show key ++ " (" ++ showGroupKey gkey ++
               "), expires at: " ++
               calendarTimeToString ltime ++ "\n"

  serveCgiMessage state hdl ShowStatus =
    reportStatus state hdl showEventHandler
   where
    showEventHandler (key,time,_,(_,_{-handler-}),gkey) = do
      ltime <- toCalendarTime time
      return $ "No. " ++ show key ++ " (" ++ showGroupKey gkey ++
               "), expires at " ++
               calendarTimeToString ltime ++ ": " ++
               --showAnyQExpression handler ++ "\n"
               "<sorry, can't show handler>\n"

  serveCgiMessage state hdl (CgiSubmit scriptenv formenv) = do
      let scriptkey = maybe "" id (lookup "SCRIPTKEY" scriptenv)
      mapIO_ (\(var,val) -> if var=="SCRIPTKEY" then done
                                                else setEnviron var val)
             scriptenv
      if null formenv -- initial form?
       then serveFormInEnv state scriptkey initform []
       else do
         (rstate,mfe) <- getNextFormAndCgiEnv state cgikey formenv
         maybe (do urlparam <- getUrlParameter
                   hPutStrLn hdl (noHandlerPage url urlparam)
                   hClose hdl
                   serveCgiMessages rstate)
               (\ (ioform,env) -> serveFormInEnv rstate scriptkey ioform env )
               mfe
   where
    serveFormInEnv rstate scriptkey hformact cenv = do
      (nstate,htmlstring) <- computeFormInStateAndEnv url cgikey fparams
                                                rstate scriptkey hformact cenv
      hPutStrLn hdl htmlstring
      hClose hdl
      serveCgiMessages nstate

  reportStatus state@(stime,maxkey,ctime,ehs) hdl eh2string = do
    lstime <- toCalendarTime stime
    lctime <- toCalendarTime ctime
    ehsstrings <- mapIO eh2string ehs
    hPutStrLn hdl $ "Started at: " ++ calendarTimeToString lstime ++ "\n" ++
                    "Next cleanup: " ++ calendarTimeToString lctime ++
                    " (maxkey: " ++ show maxkey ++")\n"++
                    "Current event handlers:\n" ++ concat ehsstrings
    hClose hdl
    serveCgiMessages state

-- computes a HTML form w.r.t. a state and a cgi environment:
computeFormInStateAndEnv
  :: String -> String -> [FormParam] -> ServerState -> String
  -> IO HtmlForm -> [(String,String)] -> IO (ServerState,String)
computeFormInStateAndEnv url cgikey fparams state scriptkey hformact cenv =
  catch tryComputeForm
        (\e -> do uparam <- getUrlParameter
                  return (state,errorAsHtml e uparam))
 where
  errorAsHtml e urlparam = addHtmlContentType $ showHtmlPage $
   page "Server Error"
    [h1 [htxt "Error: Failure during computation"],
     par [htxt "Your request cannot be processed due to a run-time error:"],
     pre [htxt (showError e)],
     par [htxt "You can try to ",
          href (url ++ if null urlparam then "" else '?':urlparam)
               [htxt "click here"],
          htxt " to try again loading the web page or inform the web ",
          htxt "administrator about this problem."]]

  tryComputeForm = do
    cform <- hformact
    let (cookiestring,hform) = extractCookies cform
    (htmlstring,evhs) <- showAnswerFormInEnv url scriptkey
                                             (addFormParams hform fparams)
                                             (getMaxFieldNr cenv + 1)
    nstate <- storeEnvHandlers state
                (formWithMultipleHandlers hform)
                (encodeKey cgikey)
                (filter (\ (t,_) -> t/="DEFAULT" && take 6 t /= "EVENT_") cenv)
                evhs
    seq (isList htmlstring) done -- to ensure to catch all failures here
    return (nstate, cookiestring++htmlstring)

  isList [] = True
  isList (_:xs) = isList xs

formWithMultipleHandlers :: HtmlForm -> Bool
formWithMultipleHandlers (HtmlAnswer _ _) = False
formWithMultipleHandlers (HtmlForm _ params _) = any isMultipleHandlers params
 where
  isMultipleHandlers formparam =
    case formparam of MultipleHandlers -> True
                      _                -> False

-- Encode an arbitrary string to make it less readable.
-- Used for encoding CGI keys before storing them on the web server.
encodeKey :: String -> String
encodeKey = map mapchr . reverse . filter (not . isSpace)
 where
  mapchr c | oc<33 || oc>126 = c
           | oc<114          = chr (oc+13)
           | otherwise       = chr (oc-81)
   where oc = ord c

-- Puts a line to stderr:
putErrLn :: String -> IO ()
putErrLn s = hPutStrLn stderr s >> hFlush stderr


--------------------------------------------------------------------------
-- Auxiliaries to implement the cgi script server:

-- get the next form and environment from a current environment (specifying a
-- user-selected event handler) and a server state holding all event handlers:
getNextFormAndCgiEnv :: ServerState -> String -> [(String,String)]
                     -> IO (ServerState, Maybe (IO HtmlForm,[(String,String)]))
getNextFormAndCgiEnv state cgikey newcenv = do
  (nstate,mbh) <- retrieveEnvHandlers state (encodeKey cgikey)
                             (urlencoded2string (getFormEvent "" newcenv))
  return $ maybe (nstate,Nothing)
                 (\ (oldcenv,handler) -> let cenv = newcenv++oldcenv in
                         (nstate, Just (handler (cgiGetValue cenv), cenv)))
                 mbh


-- put the HTML string corresponding to an HtmlForm with HTTP header on stdout:
showAnswerFormInEnv :: String -> String -> HtmlForm -> Int
                      -> IO (String,[(HtmlHandler,String)])
showAnswerFormInEnv url key hform@(HtmlForm _ _ _) crefnr = do
  (htmlstring,evhs) <- showHtmlFormInEnv url key hform crefnr
  return (addHtmlContentType htmlstring, evhs)
showAnswerFormInEnv _ _ (HtmlAnswer ctype cont) _ = do
  return ("Content-Length: " ++ show (length cont) ++
          "\nContent-Type: "++ctype++"\n\n"++cont, [])


-- Adds the initial content lines (including content length) to an HTML string.
addHtmlContentType :: String -> String
addHtmlContentType htmlstring =
    "Content-Length: " ++ show (length htmlstring) ++ "\n" ++
    "Content-Type: text/html\n\n" ++ htmlstring

-- return the HTML string corresponding to an HtmlForm:
showHtmlFormInEnv :: String -> String -> HtmlForm -> Int
                     -> IO (String,[(HtmlHandler,String)])
showHtmlFormInEnv url key (HtmlForm ftitle fparams fhexp) crefnr = do
  qstr <- getEnviron "QUERY_STRING"
  --putStrLn (showHtmlExps [pre [par (env2html cenv),hrule]]) --debug
  (title,params,hexps,firsthandler,evhs) <-
    htmlForm2html (HtmlForm ftitle fparams fhexp) crefnr
  return (showForm (if null evhs
                    then []
                    else [("SCRIPTKEY",key),("DEFAULT","EVENT_"++firsthandler)])
                   (if qstr=="" then url else url++"?"++qstr)
                   (HtmlForm title params hexps),
          evhs)


-- extract the cookies contained in a form and return the "set cookie" string
-- and the form without the cookies:
extractCookies :: HtmlForm -> (String,HtmlForm)
extractCookies (HtmlAnswer ctype cont) = ("",HtmlAnswer ctype cont)
extractCookies (HtmlForm title params hexp) =
  let cookiestring = if null cookies
                     then ""
                     else "Cache-control: no-cache=\"set-cookie\"\n" ++
                          concatMap ((++"\n") . formatCookie) cookies
   in (cookiestring, HtmlForm title otherparams hexp)
 where
   (cookies,otherparams) = splitFormParams params

   splitFormParams [] = ([],[])
   splitFormParams (fparam:fps) =
     let (cs,ops) = splitFormParams fps
      in case fparam of
           FormCookie n v ps -> ((n,v,ps):cs,ops)
           _                 -> (cs,fparam:ops)

-- get the EVENT_ definition of the cgi environment
-- (or "DEFAULT" value if it is not there):
getFormEvent :: String -> [(String,String)] -> String
getFormEvent deflt [] = deflt
getFormEvent deflt ((tag,val):tvs) =
   if tag == "DEFAULT" then getFormEvent (drop 6 val) tvs else
   if take 6 tag == "EVENT_" then urlencoded2string (drop 6 tag)
                             else getFormEvent deflt tvs

-- compute the maximal field number of all "FIELD_nr" in a CGI environment:
getMaxFieldNr :: [(String,String)] -> Int
getMaxFieldNr [] = 0
getMaxFieldNr ((name,_):env) =
  if take 6 name == "FIELD_"
  then max (tryReadNat 0 (drop 6 name)) (getMaxFieldNr env)
  else getMaxFieldNr env

-- try to read a natural number in a string or return first argument:
tryReadNat :: Int -> String -> Int
tryReadNat d s = maybe d (\(i,rs)->if null rs then i else d) (readNat s)

-- get the value assigned to a name in a given cgi environment
cgiGetValue :: [(String,String)] -> CgiRef -> String
cgiGetValue cenv cgiref =
    intercalate "\n" (map snd (filter (((idOfCgiRef cgiref) ==) . fst) cenv))

-- transform HTML form into HTML document (by instantiating CgiRefs
-- (starting with the second argument) and modifying event handlers):
-- (Result: title/HTML document/form params/encoded first handler)
htmlForm2html :: HtmlForm -> Int
             -> IO (String,[FormParam],[HtmlExp],String,[(HtmlHandler,String)])
htmlForm2html (HtmlForm title params html) crefnr = do
  let (htmlwithoutcrefs,newrefnr) = numberCgiRefs html crefnr
  -- enforce instantiation before handlers are stored:
  seq newrefnr done
  -- seq (normalForm htmlwithoutcrefs) done
  let (transhtml, evhs, fh) = translateHandlers htmlwithoutcrefs
  --storeEventHandlers cgikey oldcenv evhs
  return (title, params, transhtml, fh, evhs)


-- instantiate all CgiRefs with a unique tag in HTML expressions:
numberCgiRefs :: [HtmlExp] -> Int -> ([HtmlExp],Int)
-- arguments: HTMLExps, number for cgi-refs
-- result: translated HTMLExps, new number for cgi-refs
numberCgiRefs [] i = ([],i)
numberCgiRefs (HtmlText s : hexps) i =
  case numberCgiRefs hexps i of
    (nhexps,j) -> (HtmlText s : nhexps, j)
numberCgiRefs (HtmlStruct tag attrs hexps1 : hexps2) i =
  case numberCgiRefs hexps1 i of
    (nhexps1,j) -> case numberCgiRefs hexps2 j of
                     (nhexps2,k) -> (HtmlStruct tag attrs nhexps1 : nhexps2, k)
numberCgiRefs (HtmlEvent (HtmlStruct tag attrs hes) handler : hexps) i =
  case numberCgiRefs hexps i of
    (nhexps,j) -> (HtmlEvent (HtmlStruct tag attrs hes) handler : nhexps, j)
numberCgiRefs (HtmlCRef hexp cgiref : hexps) i
  | idOfCgiRef cgiref =:= ("FIELD_"++show i)
  = case numberCgiRefs [hexp] (i+1) of
      ([nhexp],j) -> case numberCgiRefs hexps j of
                       (nhexps,k) -> (nhexp : nhexps, k)

-- translate all event handlers into their internal form:
-- (assumption: all CgiRefs have already been instantiated and eliminated)
-- the result is the translated HTML expression list (without HtmlEvents),
-- the list of event handlers and their corresponding logical variables
-- denoting the key that is inserted for the event handler in the translated
-- HTML expression, and the string encoding of the first event handler
-- (for the default handler)
translateHandlers :: [HtmlExp] -> ([HtmlExp],[(HtmlHandler,String)],String)
translateHandlers [] = ([],[],"")
translateHandlers (HtmlText s : hexps) =
  let (nhexps,evhs,fh) = translateHandlers hexps
   in (HtmlText s : nhexps, evhs, fh)
translateHandlers (HtmlStruct tag attrs hexps1 : hexps2) =
  let (nhexps1,evhs1,fh1) = translateHandlers hexps1
      (nhexps2,evhs2,fh2) = translateHandlers hexps2
   in (HtmlStruct tag attrs nhexps1 : nhexps2, evhs1++evhs2,
       if fh1=="" then fh2 else fh1)
translateHandlers (HtmlEvent (HtmlStruct tag attrs hes) handler : hexps) =
  let (nhexps,evhs,_) = translateHandlers hexps
      fh = string2urlencoded key
   in (HtmlStruct tag (changeAssoc attrs "name" ("EVENT_" ++ fh)) hes : nhexps,
       (handler,key):evhs, fh)
 where key free

-- show a HTML form in String representation:
showForm :: [(String,String)] -> String -> HtmlForm -> String
showForm cenv url (HtmlForm title params html) =
  htmlPrelude ++
  showHtmlExp
   (HtmlStruct "html" htmlTagAttrs
     [HtmlStruct "head" []
                 ([HtmlStruct "title" [] [HtmlText (htmlQuote title)]] ++
                  concatMap param2html paramsWithEncoding),
      HtmlStruct "body" bodyattrs
       ((if null url || null cenv then id
         else \he->[HtmlStruct "form"
                               ([("method","post"),("action",url)]
                                ++ onsubmitattr ++ targetattr)
                               he])
          ( --[par (env2html cenv),hrule] ++ -- debug
           cenv2hidden cenv ++
           html))])
 where
  paramsWithEncoding = if null [e | (FormEnc e) <- params]
                       then FormEnc defaultEncoding : params
                       else params

  param2html (FormEnc enc) =
     [HtmlStruct "meta" [("http-equiv","Content-Type"),
                         ("content","text/html; charset="++enc)] []]
  param2html (FormCSS css) =
     [HtmlStruct "link" [("rel","stylesheet"),("type","text/css"),("href",css)]
                 []]
  param2html (FormMeta attrs) = [HtmlStruct "meta" attrs []]
  param2html (FormJScript js) =
     [HtmlStruct "script" [("type","text/javascript"),("src",js)] []]
  param2html (FormOnSubmit _) = []
  param2html (FormTarget _) = []
  -- no rule for FormCookie since they have been already processed
  param2html (HeadInclude hexp) = [hexp]
  param2html MultipleHandlers = []
  param2html (BodyAttr _) = []
  -- no rule for BodyAttr since it is considered later

  bodyattrs = [ps | (BodyAttr ps) <- params]

  onsubmit = [s | (FormOnSubmit s) <- params]

  onsubmitattr = if null onsubmit then [] else [("onsubmit",head onsubmit)]

  target = [s | (FormTarget s) <- params]

  targetattr = if null target then [] else [("target",head target)]


-- translate cgi environment into HTML (for debugging purposes):
env2html :: [(String,String)] -> [HtmlExp]
env2html env = concat (map (\(n,v)->[htxt (n++": "++v),breakline]) env)


-- translate environment into hidden fields (without EVENT field!):
-- (note: the field values are urlencoded to avoid problems
--  with passing special characters; moreover, the names of fields
--  containing urlencoded values are prefixed by "U")
cenv2hidden :: [(String,String)] -> [HtmlExp]
cenv2hidden env = concat (map pair2hidden env)
 where
   pair2hidden (n,v)
     | take 6 n == "EVENT_" = []
     | take 6 n == "FIELD_" = [hiddenfield ('U':n) (string2urlencoded v)]
     | otherwise            = [hiddenfield n v]

------------------------------------------------------------------------------
-- association lists (list of tag/value pairs):

-- change an associated value (or add association, if not there):
changeAssoc :: Eq tt => [(tt,tv)] -> tt -> tv -> [(tt,tv)]
changeAssoc [] tag val = [(tag,val)]
changeAssoc ((tag1,val1):tvs) tag val =
   if tag1 == tag then (tag,val) : tvs
                  else (tag1,val1) : changeAssoc tvs tag val


------------------------------------------------------------------------------
--- Execute an HTML form in "interactive" mode.
intForm :: IO HtmlForm -> IO ()
intForm = intFormMain "" "" "" "" False ""
--intcgi = intFormMain "http://localhost/~mh/" "/home/mh/public_html/" "" "fwdcgienv.cgi" False ""

--- Execute an HTML form in "interactive" mode with various parameters.
--- @param baseurl  - the base URL where this script is accessible for clients
--- @param basecgi  - the base directory in the local file system where
---                   this script should stored for execution
--- @param reldir   - the relative path added to baseurl and basecgi
--- @param cginame  - the name of the executable cgi script
--- @param forever  - True if the interactive execution should not be terminated
---                   when the final web page (without a handler) is shown
--- @param urlparam - the URL parameter for the initial call to the cgi script
--- @param hformact - IO action returning the HTML form
intFormMain :: String -> String -> String -> String ->
              Bool -> String -> IO HtmlForm -> IO ()
intFormMain baseurl basecgi reldir cginame forever urlparam hformact = do
  pid      <- getPID
  user     <- getEnviron "USER"
  home     <- getHomeDirectory
  let portname = "intcgi_" ++ show pid
  socket <- listenOn portname
  let cgiprogname = if null cginame then "cgitest_"++show pid++".cgi"
                                    else cginame
      url = (if null baseurl then "http://localhost/~"++user else baseurl)
            ++ "/" ++ reldir ++ "/" ++ cgiprogname
      cgifile = (if null basecgi then home++"/public_html/" else basecgi++"/")++
                (if null reldir  then "" else reldir ++"/") ++ cgiprogname
      cgikey = url++" 42"
  installShowCgiEnvScript portname cgifile
  setEnviron "QUERY_STRING" urlparam
  time <- getClockTime
  intFormInEnv url cgikey hformact hformact [] (initialServerState time)
               forever socket
  system ("rm "++cgifile) >> done

intFormInEnv :: String -> String -> IO HtmlForm -> IO HtmlForm
          -> [(String,String)] -> ServerState -> Bool -> Socket -> IO ()
intFormInEnv url cgikey initform hformact cenv state forever socket = do
  if null cenv then putStrLn ">>> Start initial web form..." else done
  cform <- hformact
  let (cookiestring,hform) = extractCookies cform
  (htmlstring,evhs) <- showHtmlFormInEnv url "" (extendForm hform)
                                         (getMaxFieldNr cenv + 1)
  nstate <- storeEnvHandlers state
                (formWithMultipleHandlers hform)
                (encodeKey cgikey)
                (filter (\ (t,_) -> t/="DEFAULT" && take 6 t /= "EVENT_") cenv)
                evhs
  showHtmlStringInBrowser (cookiestring++htmlstring)
  if forever || formWithHandlers hform
   then do putStrLn ">>> Waiting for web page submission..."
           (_,hdl) <- socketAccept socket
           mbmsg <- readCgiServerMsg hdl
           maybe (intFormInEnv url cgikey initform hformact
                               cenv state forever socket)
                 (intFormProceed nstate hdl)
                 mbmsg
   else putStrLn ">>> Final web page reached"
 where
   intFormProceed nstate hdl (CgiSubmit scriptenv newcenv) = do
    hPutStrLn hdl answerTxt
    hClose hdl
    mapIO_ (\ (var,val) -> setEnviron var val) scriptenv
    if null newcenv -- call to initial script?
     then intFormInEnv url cgikey initform initform [] nstate forever socket
     else do
       (rstate,mfe) <- getNextFormAndCgiEnv nstate cgikey newcenv
       maybe (putStrLn "ERROR: no submission handler")
             (\ (ioform,env) -> intFormInEnv url cgikey initform ioform
                                             env rstate forever socket)
             mfe

   answerTxt = "Content-Type: text/html\n\n" ++
                showHtmlExp (italic [htxt "Waiting for next web form..."])

   extendForm orgform =
     orgform `addFormParam` HeadInclude (HtmlStruct "base" [("href",url)] [])

-- has an HTML form event handlers?
formWithHandlers :: HtmlForm -> Bool
formWithHandlers (HtmlForm _ _ hexps) = hasHandlers hexps
 where
  hasHandlers :: [HtmlExp] -> Bool
  hasHandlers [] = False
  hasHandlers (HtmlText _ : hes) = hasHandlers hes
  hasHandlers (HtmlStruct _ _ hes1 : hes2) =
    hasHandlers hes1 || hasHandlers hes2
  hasHandlers (HtmlCRef he _ : hes) = hasHandlers [he] || hasHandlers hes
  hasHandlers (HtmlEvent _ _ : _) = True

--- Shows a string in HTML format in a browser.
showHtmlStringInBrowser :: String -> IO ()
showHtmlStringInBrowser htmlstring = do
  pid <- getPID
  let htmlfilename = "tmpcgiform_" ++ show pid ++ ".html"
  writeFile htmlfilename htmlstring
  system ("remote-netscape file:`pwd`/"++htmlfilename)
  done

-- install web script that forward user inputs:
installShowCgiEnvScript :: String -> String -> IO ()
installShowCgiEnvScript portname cgifile = do
  putStrLn ">>> Installing web script..."
  putStrLn $ "for port name: "++portname
  writeFile cgifile $ "#!/bin/sh\n"++
                      installDir++"/www/submitform \""++portname++"\"\n"
  system ("chmod 755 "++cgifile)
  done


------------------------------------------------------------------------------
-- The server for each dynamic web page manages the event handlers used in
-- dynamic web pages on the server side.
-- Each event handler is stored on the server side with a unique key.
-- Only this key is sent in the actual web page to the client.
-- Event handlers are only valid for a particular time period
-- specified by <code>eventHandlerExpiration</code>, i.e., after that time
-- event handlers will be deleted.

-- The structure of the internal state of the server:
-- Argument 1: Time when the server has been started.
-- Argument 2: Current index for numbering new events
-- Argument 3: Next date when cleanup is necessary
-- Argument 4: The current event handlers
--               (index,expiration date,cgikey,env,handler,groupindex)
--             where groupindex is Nothing for handlers with multiple use
--             and (Just gk) if the handlers should be deleted together
--             with all other handlers having the same groupindex
--             (usually, belonging to the same page)
type ServerState =
 (ClockTime, Int, ClockTime,
  [(Int,ClockTime,String,([(String,String)],HtmlHandler),Maybe Int)])

--- Creates a new state for a server started at some time.
initialServerState :: ClockTime -> ServerState
initialServerState ctime = (ctime, 0, nextCleanup ctime, [])

--- Is the list of event handlers of a server state empty?
isServerStateWithoutHandlers :: ServerState -> Bool
isServerStateWithoutHandlers (_,_,_,evhandlers) = null evhandlers

--- Gets a string describing the load of the server process.
--- If the server is "busy" it cannot accept further requests
--- for initial web pages.
getServerLoad :: ServerState -> IO String
getServerLoad (stime,maxkey,_,evs) = do
  ctime  <- getClockTime
  let busy = maxkey>500
             || (compareClockTime ctime (addMinutes 30 stime) == GT)
             || null evs -- since a server without handlers will be terminated
  return (if busy then "busy" else "ready")

--- Gets a string describing the status of the server process.
getServerStatus :: ServerState -> IO String
getServerStatus state@(stime,maxkey,_,evs) = do
  busy   <- getServerLoad state
  lstime <- toCalendarTime stime
  pinfos <- getProcessInfos
  return $ "Status: " ++ busy ++ ", Maxkey: "++show maxkey ++ ", #Handlers: " ++
           show (length evs) ++ ", Start time: " ++
           calendarTimeToString lstime ++ "\n" ++
           showMemInfo pinfos

--- Shows the group key of a handler as a string.
showGroupKey :: Maybe Int -> String
showGroupKey Nothing = "multiple use"
showGroupKey (Just gk) = "group " ++ show gk

--- Stores a list of new event handlers for a given cgi program and
--- the corresponding arguments with a new key.
--- The second argument is True if the event handlers should only be used once.
storeEnvHandlers :: ServerState -> Bool -> String -> [(String,String)]
                 -> [(HtmlHandler,String)] -> IO ServerState
storeEnvHandlers ostate multipleuse cgikey env handlerkeys = do
  time <- getClockTime
  cstate <- cleanOldEventHandlers ostate
  rannums <- getRandomSeed >>= return . drop 3 . nextInt
  let nstate = generateEventServerMessages
                 rannums
                 (if multipleuse then Nothing else Just (keyOfState cstate))
                 (eventHandlerExpiration time)
                 cstate
                 handlerkeys
  seq nstate done -- to ensure that handler keys are instantiated
  return nstate
 where
   generateEventServerMessages _ _ _ state [] = state
   generateEventServerMessages (rannum:rannums) groupkey expiredate state
                               ((handler,hkey) : evhs)
     | hkey =:= show (keyOfState state) ++ ' ':showQTerm (toUTCTime expiredate)
                ++ '_' : show rannum -- add random element to handler key string
     = generateEventServerMessages
            rannums
            groupkey
            expiredate
            (storeNewEnvEventWithCgiKey groupkey expiredate state env handler)
            evhs

   keyOfState (_,key,_,_) = key

   storeNewEnvEventWithCgiKey groupkey date (stime,maxkey,cleandate,ehs)
                              cenv info =
     (stime,
      if maxkey>30000 then 0 else maxkey+1, -- to avoid integer overflows
      cleandate,
      (maxkey,date,cgikey,(cenv,info),groupkey):ehs)

-- clean event handlers that are too old:
cleanOldEventHandlers :: ServerState -> IO ServerState
cleanOldEventHandlers state@(_,_,_,[]) = return state
cleanOldEventHandlers state@(stime,maxkey,cleandate,ehs@(_:_)) = do
  ctime <- getClockTime
  if compareClockTime ctime cleandate == LT
   then return state
   else do
     let currentehs = filter (isNotExpired ctime) ehs
         noehs = length ehs
         nocurrentehs = length currentehs
     if nocurrentehs < noehs
      then do -- report cleanup numbers:
        ltime <- toCalendarTime ctime
        putErrLn $ calendarTimeToString ltime ++ ": cleanup " ++
                   "(number of handlers: old = "++ show noehs ++ " / " ++
                   "current = "++ show nocurrentehs ++ ")"
      else done
     return (stime,maxkey, nextCleanup ctime, currentehs)
 where
  isNotExpired time (_,etime,_,_,_) = compareClockTime time etime == LT

-- Retrieves a previously stored event handler for a cgi program.
-- Returns Nothing if the handler is no longer available, i.e., expired.
retrieveEnvHandlers :: ServerState -> String -> String
                    -> IO (ServerState,Maybe ([(String,String)],HtmlHandler))
retrieveEnvHandlers state cgikey skey =
  let (numstring,datestring) = break (==' ') skey
      dateps = readsQTerm datestring
      num    = tryReadNat (-1) numstring
   in if null datestring || null dateps || num < 0
      then return (state,Nothing)
      else let (newstate,info) =
                   getEnvEventWithCgiKey state num (fst (head dateps))
            in seq newstate (return (newstate, info))
      -- the "seq"s are put here and below to enfore the evaluation of the
      -- new state in order to avoid space leaks with old, unused handlers
 where
  getEnvEventWithCgiKey oldstate@(stime,maxkey,cleandate,ehs) key date =
    maybe (oldstate,Nothing)
          (\ (evhdlr,groupkey) ->
            maybe (oldstate, Just evhdlr)
                  (\gk -> let newehs = deleteEv gk ehs
                           in seq newehs ((stime,maxkey,cleandate,newehs),
                                          Just evhdlr))
                  groupkey )
          (searchEv ehs)
   where
    -- search event handler
    searchEv [] = Nothing
    searchEv ((n,t,c,i,gk):es) =
      if key==n && date == toUTCTime t
      then if c==cgikey then Just (i,gk) else Nothing
      else searchEv es

    -- delete event handlers of the same group
    deleteEv _ [] = []
    deleteEv groupkey (ev@(_,_,_,_,Nothing):es) =
      let des = deleteEv groupkey es in seq des (ev : des)
    deleteEv groupkey (ev@(_,_,_,_,Just gk):es) =
      if groupkey==gk
      then deleteEvInGroup groupkey es
      else let des = deleteEv groupkey es in seq des (ev : des)

    deleteEvInGroup _ [] = []
    deleteEvInGroup _        (ev@(_,_,_,_,Nothing):es) = ev : es
    deleteEvInGroup groupkey (ev@(_,_,_,_,Just gk):es) =
      if groupkey==gk
      then deleteEvInGroup groupkey es
      else ev : es -- a new group has started so we stop the deletion


-- Define for a given date a new date when the event handler expires.
eventHandlerExpiration :: ClockTime -> ClockTime
eventHandlerExpiration = addHours 1
--eventHandlerExpiration = addMinutes 1

-- Define for a given date a new date when the next cleanup of event handlers
-- should be done.
nextCleanup :: ClockTime -> ClockTime
nextCleanup = addMinutes 5

---------------------------------------------------------------------------
