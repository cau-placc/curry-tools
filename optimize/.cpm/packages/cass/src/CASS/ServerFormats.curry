------------------------------------------------------------------------------
--- This module defines the various output formats offered by the
--- anlysis server.
---
--- @author Heiko Hoffmann, Michael Hanus
--- @version April 2024
------------------------------------------------------------------------------

module CASS.ServerFormats
  ( OutputFormat(..), serverFormatNames, formatResult )
 where

import Data.List         ( sortBy )

import FlatCurry.Types   ( QName, showQNameInModule )
import JSON.Data
import JSON.Pretty       ( ppJSON )
import XML

import Analysis.ProgInfo

------------------------------------------------------------------------------
--- The type of supported output formats of the analysis server.
data OutputFormat =
  FormatText | FormatShort | FormatTerm | FormatJSON | FormatJSONTerm |
  FormatXML
 deriving Eq

--- The names and output formats supported by the analysis server.
serverFormatNames :: [(String,OutputFormat)]
serverFormatNames =
  [ ("text",FormatText), ("short",FormatShort), ("curryterm",FormatTerm)
  , ("json", FormatJSON), ("jsonterm", FormatJSONTerm), ("xml",FormatXML)]

--- Format an analysis result in different formats.
--- The arguments are the module name, the output format,
--- `(Just n)` if not the complete module but the result for entity `n`
--- should only be shown, and a flag which is true if only the interface
--- information should be shown.
formatResult :: String -> OutputFormat -> Maybe String -> Bool
             -> (Either (ProgInfo String) String) -> String
formatResult _ outForm _ _ (Right err) =
  case outForm of FormatXML      -> showXmlDoc (xml "error" [xtxt errMsg])
                  FormatJSON     -> ppJSON (JString errMsg)
                  FormatJSONTerm -> ppJSON (JString errMsg)
                  _              -> errMsg
 where errMsg = "ERROR in analysis: " ++ err
-- Format a single program entity result:
formatResult moduleName outForm (Just name) _ (Left pinfo) =
  case lookupProgInfo (moduleName,name) pinfo of
    Nothing    -> "ERROR " ++ name ++ " not found in " ++ moduleName
    Just value -> case outForm of
                    FormatXML      -> showXmlDoc (xml "result" [xtxt value])
                    FormatJSON     -> ppJSON (JString value)
                    FormatJSONTerm -> ppJSON (JString value)
                    _              -> value
-- Format a complete module:
formatResult moduleName outForm Nothing public (Left pinfo) =
  case outForm of
    FormatTerm     -> show entities ++ "\n"
    FormatXML      -> showXmlDoc $ xml "results" $ map entry2xml entities
    FormatJSON     -> entitiesAsJSON
    FormatJSONTerm -> entitiesAsJSON
    _              -> formatAsText moduleName entities
 where
  (pubents,privents) = progInfo2Lists pinfo

  entities = if public then pubents
                       else sortBy (\ (qf1,_) (qf2,_) -> qf1 <= qf2)
                                   (pubents ++ privents)

  entry2xml ((mname,name),value) =
    xml "operation" [xml "module" [xtxt mname],
                     xml "name"   [xtxt name],
                     xml "result" [xtxt value]]

  entitiesAsJSON = ppJSON (JArray $ map entry2json entities) ++ "\n"

  entry2json ((mname,name),value) =
    JObject [("module", JString mname),
             ("name"  , JString name),
             ("result", JString value)]

-- Format a list of analysis results as a string (lines of analysis results).
formatAsText :: String -> [(QName,String)] -> String
formatAsText moduleName =
  unlines . map (\ (qf,r) -> showQNameInModule moduleName qf ++ " : " ++ r)

------------------------------------------------------------------------------
