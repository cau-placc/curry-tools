-----------------------------------------------------------------------
--- This module defines a datatype to represent the analysis information.
---
--- @author Heiko Hoffmann, Michael Hanus
--- @version April 2021
-----------------------------------------------------------------------

module Analysis.ProgInfo
  ( ProgInfo, emptyProgInfo, lookupProgInfo, combineProgInfo
  , lists2ProgInfo, publicListFromProgInfo, progInfo2Lists
  , mapProgInfo, publicProgInfo
  , showProgInfo, equalProgInfo
  , readAnalysisFiles, readAnalysisPublicFile, writeAnalysisFiles
  ) where

import Prelude hiding   ( empty, lookup )
import System.Directory ( removeFile )
import System.FilePath  ( (<.>) )

import Data.Map
import FlatCurry.Types
import XML

import Analysis.Logging ( DLevel, debugMessage )

--- Type to represent analysis information.
--- The first component are public declarations, the second the private ones.
data ProgInfo a = ProgInfo (Map QName a) (Map QName a)

--- The empty program information.
emptyProgInfo:: ProgInfo a
emptyProgInfo = ProgInfo empty empty

--- Gets the information about an entity.
lookupProgInfo:: QName -> ProgInfo a -> Maybe a
lookupProgInfo key (ProgInfo map1 map2) =
 case lookup key map1 of
  Just x -> Just x
  Nothing ->  lookup key map2

--- Combines two analysis informations.
combineProgInfo :: ProgInfo a -> ProgInfo a -> ProgInfo a
combineProgInfo (ProgInfo x1 x2) (ProgInfo y1 y2) =
  ProgInfo (union x1 y1) (union x2 y2)

--- Converts a public and a private analysis list into a program info.
lists2ProgInfo :: ([(QName,a)],[(QName,a)]) -> ProgInfo a
lists2ProgInfo (xs,ys) = ProgInfo (fromList xs) (fromList ys)

--- Returns the infos of public operations as a list.
publicListFromProgInfo:: ProgInfo a -> [(QName,a)]
publicListFromProgInfo (ProgInfo fm1 _) = toList fm1

--- Transforms a program information into a pair of lists
--- containing the information about public and private entities.
progInfo2Lists :: ProgInfo a -> ([(QName,a)],[(QName,a)])
progInfo2Lists (ProgInfo map1 map2) = (toList map1, toList map2)

--- Transforms a program information by applying a function to all
--- information entities.
mapProgInfo:: (a -> b) -> ProgInfo a -> ProgInfo b
mapProgInfo func (ProgInfo map1 map2) =
  ProgInfo (mapWithKey (\_ b->func b) map1) (mapWithKey (\_ b->func b) map2)

--- Transforms a program information into a program information
--- about interface entities only.
publicProgInfo :: ProgInfo a -> ProgInfo a
publicProgInfo (ProgInfo pub _) = ProgInfo pub empty

--- Show a ProgInfo as a string (used for debugging only).
showProgInfo :: Show a => ProgInfo a -> String
showProgInfo (ProgInfo fm1 fm2) =
  "Public: "++show fm1++"\nPrivate: "++show fm2

-- Equality on ProgInfo
equalProgInfo :: Eq a => ProgInfo a -> ProgInfo a -> Bool
equalProgInfo (ProgInfo pi1p pi1v) (ProgInfo pi2p pi2v) =
   pi1p == pi2p && pi1v == pi2v

--- Writes a ProgInfo into a file.
writeAnalysisFiles :: Show a => DLevel -> String -> ProgInfo a -> IO ()
writeAnalysisFiles dl basefname (ProgInfo pub priv) = do
  debugMessage dl 3 $ "Writing analysis files '"++basefname++"'..."
  writeFile (basefname <.> "priv") (show priv)
  writeFile (basefname <.> "pub")  (show pub)

--- Reads a ProgInfo from the analysis files where the base file name is given.
readAnalysisFiles :: Read a => DLevel -> String -> IO (ProgInfo a)
readAnalysisFiles dl basefname = do
  debugMessage dl 3 $ "Reading analysis files '"++basefname++"'..."
  let pubcontfile  = basefname <.> "pub"
      privcontfile = basefname <.> "priv"
  pubcont  <- readFile pubcontfile
  privcont <- readFile privcontfile
  let pinfo = ProgInfo (read pubcont) (read privcont)
  catch (return $!! pinfo)
        (\err -> do
           putStrLn ("Buggy analysis files detected and removed:\n"++
                     basefname)
           mapM_ removeFile [pubcontfile,privcontfile]
           putStrLn "Please try to re-run the analysis!"
           ioError err)

--- Reads the public ProgInfo from the public analysis file.
readAnalysisPublicFile :: Read a => DLevel -> String -> IO (ProgInfo a)
readAnalysisPublicFile dl fname = do
  debugMessage dl 3 $ "Reading public analysis file '"++fname++"'..."
  fcont <- readFile fname
  let pinfo = ProgInfo (read fcont) empty
  catch (return $!! pinfo)
        (\err -> do
           putStrLn ("Buggy analysis files detected and removed:\n"++fname)
           removeFile fname
           putStrLn "Please try to re-run the analysis!"
           ioError err)

-----------------------------------------------------------------------
