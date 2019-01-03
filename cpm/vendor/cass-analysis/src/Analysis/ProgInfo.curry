-----------------------------------------------------------------------
--- This module defines a datatype to represent the analysis information.
---
--- @author Heiko Hoffmann, Michael Hanus
--- @version January 2019
-----------------------------------------------------------------------

module Analysis.ProgInfo
  ( ProgInfo, emptyProgInfo, lookupProgInfo, combineProgInfo
  , lists2ProgInfo, publicListFromProgInfo, progInfo2Lists, progInfo2XML
  , mapProgInfo, publicProgInfo
  , showProgInfo, equalProgInfo
  , readAnalysisFiles, readAnalysisPublicFile, writeAnalysisFiles
  ) where

import Directory     (removeFile)
import FilePath      ((<.>))

import Data.FiniteMap
import FlatCurry.Types
import XML

import Analysis.Logging (debugMessage)

--- Type to represent analysis information.
--- The first component are public declarations, the second the private ones.
data ProgInfo a = ProgInfo (FM QName a) (FM QName a)

--- The empty program information.
emptyProgInfo:: ProgInfo a
emptyProgInfo = ProgInfo (emptyFM (<)) (emptyFM (<))

--- Gets the information about an entity.
lookupProgInfo:: QName -> ProgInfo a -> Maybe a
lookupProgInfo key (ProgInfo map1 map2) =
 case lookupFM map1 key of
  Just x -> Just x
  Nothing ->  lookupFM map2 key

--- Combines two analysis informations.
combineProgInfo :: ProgInfo a -> ProgInfo a -> ProgInfo a
combineProgInfo (ProgInfo x1 x2) (ProgInfo y1 y2) =
  ProgInfo (plusFM x1 y1) (plusFM x2 y2)

--- Converts a public and a private analysis list into a program info.
lists2ProgInfo :: ([(QName,a)],[(QName,a)]) -> ProgInfo a
lists2ProgInfo (xs,ys) = ProgInfo (listToFM (<) xs) (listToFM (<) ys)

--- Returns the infos of public operations as a list.
publicListFromProgInfo:: ProgInfo a -> [(QName,a)]
publicListFromProgInfo (ProgInfo fm1 _) = fmToList fm1

--- Transforms a program information into a pair of lists
--- containing the information about public and private entities.
progInfo2Lists :: ProgInfo a -> ([(QName,a)],[(QName,a)])
progInfo2Lists (ProgInfo map1 map2)= (fmToList map1,fmToList map2)

--- Transforms analysis information into XML format.
progInfo2XML :: ProgInfo String -> ([XmlExp],[XmlExp])
progInfo2XML (ProgInfo map1 map2) = 
  (foldFM entry2xml [] map1, foldFM entry2xml [] map2)
 where
  entry2xml (mname,name) value xmlList =
    (xml "operation" [xml "module" [xtxt mname],
                      xml "name"   [xtxt name],
                      xml "result" [xtxt value]]) : xmlList 

mapProgInfo:: (a->b) -> ProgInfo a -> ProgInfo b
mapProgInfo func (ProgInfo map1 map2) = 
  ProgInfo (mapFM (\_ b->func b) map1) (mapFM (\_ b->func b) map2)

--- Transforms a program information into a program information
--- about interface entities only.
publicProgInfo :: ProgInfo a -> ProgInfo a
publicProgInfo (ProgInfo pub _) = ProgInfo pub (emptyFM (<))

--- Show a ProgInfo as a string (used for debugging only).
showProgInfo :: ProgInfo _ -> String
showProgInfo (ProgInfo fm1 fm2) =
  "Public: "++showFM fm1++"\nPrivate: "++showFM fm2

-- Equality on ProgInfo
equalProgInfo :: Eq a => ProgInfo a -> ProgInfo a -> Bool
equalProgInfo (ProgInfo pi1p pi1v) (ProgInfo pi2p pi2v) =
  eqFM pi1p pi2p && eqFM pi1v pi2v

--- Writes a ProgInfo into a file.
writeAnalysisFiles :: String -> ProgInfo _ -> IO ()
writeAnalysisFiles basefname (ProgInfo pub priv) = do
  debugMessage 3 $ "Writing analysis files '"++basefname++"'..."
  writeFile (basefname <.> "priv") (showFM priv)
  writeFile (basefname <.> "pub")  (showFM pub)

--- Reads a ProgInfo from the analysis files where the base file name is given.
readAnalysisFiles :: String -> IO (ProgInfo _)
readAnalysisFiles basefname = do
  debugMessage 3 $ "Reading analysis files '"++basefname++"'..."
  let pubcontfile  = basefname <.> "pub"
      privcontfile = basefname <.> "priv"
  pubcont  <- readFile pubcontfile
  privcont <- readFile privcontfile
  let pinfo = ProgInfo (readFM (<) pubcont) (readFM (<) privcont)
  catch (return $!! pinfo)
        (\err -> do
           putStrLn ("Buggy analysis files detected and removed:\n"++
                     basefname)
           mapIO_ removeFile [pubcontfile,privcontfile]
           putStrLn "Please try to re-run the analysis!"
           ioError err)

--- Reads the public ProgInfo from the public analysis file.
readAnalysisPublicFile :: String -> IO (ProgInfo _)
readAnalysisPublicFile fname = do
  debugMessage 3 $ "Reading public analysis file '"++fname++"'..."
  fcont <- readFile fname
  let pinfo = ProgInfo (readFM (<) fcont) (emptyFM (<))
  catch (return $!! pinfo)
        (\err -> do
           putStrLn ("Buggy analysis files detected and removed:\n"++fname)
           removeFile fname
           putStrLn "Please try to re-run the analysis!"
           ioError err)

-----------------------------------------------------------------------
