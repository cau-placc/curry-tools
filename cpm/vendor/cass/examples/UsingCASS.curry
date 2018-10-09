--- A simple program to show the usage of the API mode of CASS
--- to access the demanded values of the operation Rev.rev:

import CASS.Server           ( analyzeGeneric )
import Analysis.ProgInfo     ( lookupProgInfo )
import Analysis.Demandedness ( demandAnalysis )

demandedArgumentsOf :: String -> String -> IO [Int]
demandedArgumentsOf modname fname = do
  deminfo <- analyzeGeneric demandAnalysis modname >>= return . either id error
  return $ maybe [] id (lookupProgInfo (modname,fname) deminfo)

main :: IO [Int]
main = demandedArgumentsOf "Rev" "rev"
