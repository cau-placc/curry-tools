--------------------------------------------------------------------------
--- This module contains operations to deal with the documentation
--- of analyses registered in CASS.
---
--- @author Michael Hanus
--- @version July 2016
--------------------------------------------------------------------------

module CASS.Doc(getAnalysisDoc) where

import System.Directory   (doesFileExist)
import System.FilePath    ((</>), (<.>))

import CASS.Configuration (docDir)

--------------------------------------------------------------------------
--- Gets the documentation of an analysis with a registered name.
--- Returns `Nothing` if no documentation exist.
--- The documentation of an analysis with name AN is usually stored
--- in the file `<PACKAGEROOT>/docs/AN.md`.
getAnalysisDoc :: String -> IO (Maybe String)
getAnalysisDoc aname = do
  let docfilename = docDir </> aname <.> "md"
  docfileexists <- doesFileExist docfilename
  if docfileexists then readFile docfilename >>= return . Just
                   else return Nothing

--------------------------------------------------------------------------
