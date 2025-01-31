-----------------------------------------------------------------------
--- This module defines a datatype to represent the analysis information.
---
--- @author Heiko Hoffmann, Michael Hanus
--- @version January 2025
-----------------------------------------------------------------------
{-# OPTIONS_FRONTEND -Wno-incomplete-patterns #-}

module Analysis.ProgInfo
  ( ProgInfo, emptyProgInfo, lookupProgInfo, combineProgInfo
  , lists2ProgInfo, publicListFromProgInfo, progInfo2Lists
  , mapProgInfo, publicProgInfo
  , showProgInfo, equalProgInfo
  , readAnalysisFiles, readAnalysisPublicFile, readAnalysisPrivateFile, readAnalysisFile
  , writeAnalysisFiles
  ) where

import Prelude hiding   ( empty, lookup )

import Debug.Profile    ( getElapsedTimeNF )
import Data.Map
import Data.Time        ( compareClockTime )
import FlatCurry.Types
import RW.Base
import System.Directory ( doesFileExist, getModificationTime, removeFile )
import System.FilePath  ( (<.>) )
import System.IO        ( hPutChar )

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
writeAnalysisFiles :: (ReadWrite a, Show a) => DLevel -> String -> ProgInfo a
                   -> IO ()
writeAnalysisFiles dl basefname (ProgInfo pub priv) = do
  debugMessage dl 3 $ "Writing analysis files '" ++ basefname ++ "'..."
  writeTermFile dl (basefname <.> "priv") priv
  writeTermFile dl (basefname <.> "pub" )  pub

--- Reads a ProgInfo from the analysis files where the base file name is given.
readAnalysisFiles :: (Read a, ReadWrite a) => DLevel -> String
                  -> IO (ProgInfo a)
readAnalysisFiles dl basefname = do
  debugMessage dl 3 $ "Reading analysis files '" ++ basefname ++ "'..."
  let pubcontfile  = basefname <.> "pub"
      privcontfile = basefname <.> "priv"
  pubinfo  <- readTermFile (fromEnum dl > 2) pubcontfile
  privinfo <- readTermFile (fromEnum dl > 2) privcontfile
  let pinfo = ProgInfo pubinfo privinfo
  catch (return $!! pinfo)
        (\err -> do
           putStrLn $ "Buggy analysis files detected and removed:\n" ++
                      basefname
           mapM_ removeFile [pubcontfile,privcontfile]
           putStrLn "Please try to re-run the analysis!"
           ioError err)

--- Reads the public `ProgInfo` from an existing public analysis file and
--- set the private information to empty.
--- If the file is buggy, an error is raised.
readAnalysisPublicFile :: (Read a, ReadWrite a) => DLevel -> String
                       -> IO (ProgInfo a)
readAnalysisPublicFile dl fname = do
  debugMessage dl 3 $ "Reading public analysis file '" ++ fname ++ "'..."
  infomap <- readAnalysisFile dl fname
  return $ ProgInfo infomap empty

--- Reads the private `ProgInfo` from a private analysis file and
--- set the public information to empty. If the private analysis file
--- does not exist, the empty `ProgInfo` is returned.
--- If the file is buggy, an error is raised.
readAnalysisPrivateFile :: (Read a, ReadWrite a) => DLevel -> String
                        -> IO (ProgInfo a)
readAnalysisPrivateFile dl fname = do
  debugMessage dl 3 $ "Reading private analysis file '" ++ fname ++ "'..."
  fexists <- doesFileExist fname
  infomap <- if fexists then readAnalysisFile dl fname else return empty
  return $ ProgInfo empty infomap

--- Reads an existing file with (public or private) analysis information.
readAnalysisFile :: (Read a, ReadWrite a) => DLevel -> String
                 -> IO (Map QName a)
readAnalysisFile dl fname = do
  infomap <- readTermFile (fromEnum dl > 2) fname
  catch (return $!! infomap)
        (\err -> do
           putStrLn $ "Buggy analysis files detected and removed:\n" ++ fname
           removeFile fname
           putStrLn "Please try to re-run the analysis!"
           ioError err)

------------------------------------------------------------------------------
-- Reading/writing data files.

--- Writes data as a term and a compact term into a file.
writeTermFile :: (ReadWrite a, Show a) => DLevel -> String -> a -> IO ()
writeTermFile _ fname x = do
  writeFile fname (show x)
  writeDataFile (fname <.> "rw") x

--- Reads data in term representation from a file.
--- Try to read compact representation if it exists and
--- is not older than the term file.
--- If the first argument is `True`, read also the term file and report
--- the timings of reading this file and the compact data file.
readTermFile :: (ReadWrite a, Read a) => Bool -> String -> IO a
readTermFile reporttimings fname = do
  let rwfile = fname <.> "rw"
      readtermfile = fmap read (readFile fname)
  rwex <- doesFileExist rwfile
  if rwex
    then do
      ftime   <- getModificationTime fname
      rwftime <- getModificationTime rwfile
      if compareClockTime rwftime ftime == LT
        then readtermfile
        else do
          (mbterms,rwtime) <- getElapsedTimeNF (readDataFile rwfile)
          maybe (error $ "Illegal data in file " ++ rwfile)
                (\rwterms ->
                  if not reporttimings
                    then return rwterms
                    else do
                      putStrLn $ "\nReading " ++ fname
                      (_,ttime) <- getElapsedTimeNF readtermfile
                      putStrLn $ "Time: " ++ show ttime ++
                                 " msecs / Compact reading: " ++
                                 show rwtime ++ " msecs" ++
                                 (if rwtime == 0
                                    then ""
                                    else " / speedup: " ++
                                         show (fromInt ttime / fromInt rwtime))
                      return rwterms )
                mbterms
    else readtermfile

------------------------------------------------------------------------------
--- `ReadWrite` instance of `Map`.
instance (ReadWrite a,ReadWrite b) => ReadWrite (Map a b) where
  readRW _    ('0' : r0) = (Tip,r0)
  readRW strs ('1' : r0) = (Bin a' b' c' d' e',r5)
    where
      (a',r1) = readRW strs r0
      (b',r2) = readRW strs r1
      (c',r3) = readRW strs r2
      (d',r4) = readRW strs r3
      (e',r5) = readRW strs r4

  showRW _      strs0 Tip = (strs0,showChar '0')
  showRW params strs0 (Bin a' b' c' d' e') =
    (strs5,showChar '1' . (show1 . (show2 . (show3 . (show4 . show5)))))
    where
      (strs1,show1) = showRW params strs0 a'
      (strs2,show2) = showRW params strs1 b'
      (strs3,show3) = showRW params strs2 c'
      (strs4,show4) = showRW params strs3 d'
      (strs5,show5) = showRW params strs4 e'

  writeRW _      h Tip strs = hPutChar h '0' >> return strs
  writeRW params h (Bin a' b' c' d' e') strs =
    hPutChar h '1'
     >> ((((writeRW params h a' strs >>= writeRW params h b')
            >>= writeRW params h c')
           >>= writeRW params h d')
          >>= writeRW params h e')

  typeOf n = RWType "Map" [typeOf (get_a n),typeOf (get_b n)]
    where
      get_a :: Map a' b' -> a'
      get_a _ = failed
      get_b :: Map a' b' -> b'
      get_b _ = failed

------------------------------------------------------------------------------
