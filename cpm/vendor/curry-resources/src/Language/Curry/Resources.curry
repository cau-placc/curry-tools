------------------------------------------------------------------------------
--- Definition of various URLs to resources related to Curry,
--- like the Curry home page, the documentation site for packages, etc.
--- These URLs can be used in Curry applications which generate
--- documentations related to Curry, e.g., program documentations (CurryDoc),
--- package descriptions, etc.
------------------------------------------------------------------------------

module Language.Curry.Resources where

--- The URL of the home page of Curry.
curryHomeURL :: String
curryHomeURL = "http://www.curry-lang.org"

--- The URL of the home page of CPM containing a description and
--- some documentation about CPM.
cpmHomeURL :: String
cpmHomeURL = "http://www.curry-lang.org/tools/cpm"

--- The URL of the documentation of all published packages.
--- It contains the overview and index of all published Curry packages.
curryPackagesURL :: String
curryPackagesURL = "https://cpm.curry-lang.org/"

--- The base URL of the documentation of individual published packages.
---
--- The description of the latest version of a package with name `pn`
--- can can be found at
---
---     curryPackageBaseURL ++ pn ++ ".html"
---
--- The description of a package with name `pn` and version `pv`
--- can can be found at
---
---     curryPackageBaseURL ++ pn ++ "-" ++ pv ++ ".html"
---
--- See also operations 'curryPackageLatestURL' and 'curryPackageURL'.
curryPackageBaseURL :: String
curryPackageBaseURL = curryPackagesURL ++ "pkgs/"

--- The URL of the documentation of the latest version of a published package
--- where the name is given as the argument.
curryPackageLatestURL :: String -> String
curryPackageLatestURL pn = curryPackageBaseURL ++ pn ++ ".html"

--- The URL of the documentation of a published package where
--- the name and version are given as the argument, e.g.,
--- the expression `curryPackageURL "flatcurry" "3.0.0"` evaluates to the
--- URL of package `flatcurry` in version `3.0.0`.
curryPackageURL :: String -> String -> String
curryPackageURL pn pv = curryPackageBaseURL ++ pn ++ "-" ++ pv ++ ".html"

--- The URL of the documentation of the latest version of the
--- base libraries of Curry systems.
baseLibsURL :: String
baseLibsURL = curryPackageLatestURL "base"

--- The base URL of API documentations of published packages.
--- The actual API documentation of a package with name `pn` and version `pv`
--- can be found at
---
---     cpmPackagesDocURL ++ pn ++ "-" ++ pv
---
--- See also operation 'curryPackageDocURL'.
curryPackagesDocURL :: String
curryPackagesDocURL = curryPackagesURL ++ "DOC/"

--- The URL of the API documentation of a package where the name and version
--- are given as the arguments, e.g., `curryPackageDocURL "flatcurry" "3.0.0"`.
curryPackageDocURL :: String -> String -> String
curryPackageDocURL pn pv = curryPackagesDocURL ++ pn ++ "-" ++ pv


--- The URL of the API search engine.
currygleURL :: String
currygleURL = "https://www-ps.informatik.uni-kiel.de/kics2/currygle/"

--- The URL of the Curry Wiki.
curryWikiURL :: String
curryWikiURL = "https://www-ps.informatik.uni-kiel.de/currywiki"

------------------------------------------------------------------------------
-- URL of Curry systems:

--- The home page of the Curry system PAKCS.
pakcsURL :: String
pakcsURL = "https://www.informatik.uni-kiel.de/~pakcs/"

--- The home page of the Curry system KiCS2.
kics2URL :: String
kics2URL = "https://www-ps.informatik.uni-kiel.de/kics2/"

--- The home page of the Curry system Curry2Go.
curry2goURL :: String
curry2goURL = "https://www-ps.informatik.uni-kiel.de/curry2go/"

------------------------------------------------------------------------------
