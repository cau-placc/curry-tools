--- Analyzing the residuation behavior of a module and returns
--- some statistical information.

import Data.List ( intercalate, partition )

import FlatCurry.Types ( QName )
import CASS.Server           ( analyzeGeneric )
import Analysis.ProgInfo     ( progInfo2Lists )
import Analysis.Residuation -- ( demandAnalysis )

residuationInfoOf :: String
                  -> IO ([(QName,ResiduationInfo)],[(QName,ResiduationInfo)])
residuationInfoOf modname = do
  analyzeGeneric residuationAnalysis modname
    >>= return . either progInfo2Lists error

countResOps :: String -> IO [String]
countResOps mname = do
  putStrLn $ "Analyzing module " ++ mname ++ "..."
  (pubres,privres) <- residuationInfoOf mname
  let (resops,nonresops) = partition (\(_,i) -> i==MayResiduate || i==NoResInfo)
                                     (pubres ++ privres)
  return [mname, show (length resops), show (length nonresops)]

printCountResOps :: [String] -> IO ()
printCountResOps mname = do
  stats <- mapIO countResOps mname
  putStrLn $ "Module | Residuating | Non-residuating"
  mapIO_ (\row -> putStrLn (intercalate "|" row)) stats

main :: IO ()
main = printCountResOps baseModules

baseModules :: [String]
baseModules = ["Prelude","List","Char"]

allBaseModules :: [String]
allBaseModules =
  ["AllSolutions"
  ,"AnsiCodes"
  ,"Char"
  ,"Combinatorial"
  ,"CPNS"
  ,"Debug"
  ,"Dequeue"
  ,"Directory"
  ,"Distribution"
  ,"Either"
  ,"ErrorState"
  ,"FileGoodies"
  ,"FilePath"
  ,"Findall"
  ,"FiniteMap"
  ,"Float"
  ,"Format"
  ,"Function"
  ,"FunctionInversion"
  ,"GetOpt"
  ,"Global"
  ,"Integer"
  ,"IO"
  ,"IOExts"
  ,"List"
  ,"Maybe"
  ,"NamedSocket"
  ,"Nat"
  ,"Prelude"
  ,"Profile"
  ,"PropertyFile"
  ,"Random"
  ,"Read"
  ,"ReadNumeric"
  ,"ReadShowTerm"
  ,"RedBlackTree"
  ,"SearchTree"
  ,"SearchTreeGenerators"
  ,"SearchTreeTraversal"
  ,"SetFunctions"
  ,"SetRBT"
  ,"ShowS"
  ,"Socket"
  ,"Sort"
  ,"State"
  ,"System"
  ,"TableRBT"
  ,"Time"
  ,"Traversal"
  ,"Unsafe"
  ,"ValueSequence"
  ]
