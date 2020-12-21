module CASS.PackageConfig where

import Curry.Compiler.Distribution ( installDir )
import System.FilePath             ( (</>) )

--- Package version as a string.
packageVersion :: String
packageVersion = "3.0.0"

--- Package location.
packagePath :: String
packagePath =
 installDir </> "currytools" </> "optimize" </> ".cpm" </> "packages" </> "cass"

--- Location of the executable installed by this package.
packageExecutable :: String
packageExecutable = ""

--- Load path for the package (if it is the main package).
packageLoadPath :: String
packageLoadPath = ""
