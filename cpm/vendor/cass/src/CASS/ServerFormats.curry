--------------------------------------------------------------------
--- This module defines the various output formats offered by the
--- anlysis server.
---
--- @author Heiko Hoffmann, Michael Hanus
--- @version January 2017
--------------------------------------------------------------------

module CASS.ServerFormats(serverFormats,formatResult) where

import Analysis.ProgInfo
import FlatCurry.Types   ( QName, showQNameInModule )
import Data.List         ( sortBy )
import XML

--------------------------------------------------------------------
--- The supported formats of the analysis server:
serverFormats :: [String]
serverFormats = ["XML","CurryTerm","Text"]

--- Format an analysis result in different formats.
--- The arguments are the module name, the output format (see 'serverFormats'),
--- `(Just n)` if not the complete module but the result for entity `n`
--- should only be shown, and a flag which is true if only the interface
--- information should be shown.
formatResult :: String -> String -> Maybe String -> Bool
             -> (Either (ProgInfo String) String) -> String
formatResult _ outForm _ _ (Right err) =
  let errMsg = "ERROR in analysis: " ++ err
   in if outForm == "XML"
      then showXmlDoc (xml "error" [xtxt errMsg])
      else errMsg
-- Format a single program entity result:
formatResult moduleName outForm (Just name) _ (Left pinfo) =
  let lookupResult = lookupProgInfo (moduleName,name) pinfo
   in case lookupResult of
        Nothing -> ("ERROR "++name++" not found in "++moduleName)
        Just value ->
          case outForm of
           "CurryTerm" -> value
           "Text"      -> value
           "XML"       -> showXmlDoc (xml "result" [xtxt value])
           _ -> error "Internal error ServerFormats.formatResult"
-- Format a complete module:
formatResult moduleName outForm Nothing public (Left pinfo) =
  case outForm of
    "CurryTerm" -> show entities
    "Text"      -> formatAsText moduleName entities
    "XML"       -> let (pubxml,privxml) = progInfo2XML pinfo
                    in showXmlDoc
                        (xml "results"
                          (pubxml ++ if public then [] else privxml))
    _ -> error "Internal error ServerFormats.formatResult"
 where
   entities = let (pubents,privents) = progInfo2Lists pinfo
               in if public then pubents
                            else sortBy (\ (qf1,_) (qf2,_) -> qf1<=qf2)
                                        (pubents++privents)

-- Format a list of analysis results as a string (lines of analysis results).
formatAsText :: String -> [(QName,String)] -> String
formatAsText moduleName =
  unlines . map (\ (qf,r) -> showQNameInModule moduleName qf ++ " : " ++ r)

--------------------------------------------------------------------
