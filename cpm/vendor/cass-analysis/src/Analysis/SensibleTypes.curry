------------------------------------------------------------------------------
--- A type is sensible if there exists at least one value of this type.
--- This module contains an analysis which associates to each type
--- constructor the following information:
--- * sensible, i.e., there is always some value of this type
--- * parametric sensible, i.e., it is sensible of all type arguments
---   are instantiated with sensible types
--- * not sensible, i.e., maybe not sensible
------------------------------------------------------------------------------

module Analysis.SensibleTypes
  ( Sensible(..), showSensible, sensibleType )
 where

import Data.Maybe
import FlatCurry.Types
import FlatCurry.Goodies
import RW.Base
import System.IO

import Analysis.Types
import Analysis.ProgInfo

------------------------------------------------------------------------------
--- Datatype to represent sensible type information.
data Sensible = NotSensible | PSensible | Sensible
  deriving (Show, Read, Eq)

-- Show higher-order information as a string.
showSensible :: AOutFormat -> Sensible -> String
showSensible _ Sensible    = "sensible"
showSensible _ PSensible   = "parametric sensible"
showSensible _ NotSensible = "not sensible"

lubSens :: Sensible -> Sensible -> Sensible
lubSens Sensible    _           = Sensible
lubSens PSensible   Sensible    = Sensible
lubSens PSensible   PSensible   = PSensible
lubSens PSensible   NotSensible = PSensible
lubSens NotSensible x           = x

------------------------------------------------------------------------
-- Analysis of sensible types

sensibleType :: Analysis Sensible
sensibleType = dependencyTypeAnalysis "SensibleType" NotSensible sensOfType

-- predefined sensible data types
predefinedSensibles :: [QName]
predefinedSensibles = [pre "Int", pre "Float", pre "Char", pre "IO"]
 where pre tc = ("Prelude",tc)

sensOfType :: TypeDecl -> [(QName,Sensible)] -> Sensible
sensOfType (TypeSyn _ _ _ typeExpr) usedtypes =
  sensOfTypeExpr usedtypes typeExpr
sensOfType (TypeNew _ _ _ (NewCons _ _ typeExpr)) usedtypes =
  sensOfTypeExpr usedtypes typeExpr
sensOfType (Type tc _ _ conDecls) usedtypes
  | tc `elem` predefinedSensibles = Sensible
  | otherwise = foldr lubSens NotSensible (map sensOfConsDecl conDecls)
 where
   sensOfConsDecl (Cons _ _ _ typeExprs)
     | all (== Sensible)    senstargs = Sensible
     | all (/= NotSensible) senstargs = PSensible
     | otherwise                      = NotSensible
    where senstargs = map (sensOfTypeExpr usedtypes) typeExprs

-- Compute the sensibility of a type expression which depends on the
-- information about type cosntructors.
sensOfTypeExpr :: [(QName,Sensible)] -> TypeExpr -> Sensible
sensOfTypeExpr _ (TVar _)       = PSensible
sensOfTypeExpr _ (FuncType _ _) = NotSensible -- we do not know which functions
                                              -- of some type exists...
sensOfTypeExpr usedtypes (TCons tc typeExprs)
  | senstc == Sensible || (senstc == PSensible && all (==Sensible) senstargs)
  = Sensible
  | senstc == PSensible && all (/=NotSensible) senstargs
  = PSensible
  | otherwise
  = NotSensible
 where
  senstc    = maybe NotSensible id (lookup tc usedtypes)
  senstargs = map (sensOfTypeExpr usedtypes) typeExprs
sensOfTypeExpr usedtypes (ForallType _ texp) = sensOfTypeExpr usedtypes texp

------------------------------------------------------------------------------
-- ReadWrite instances:

instance ReadWrite Sensible where
  readRW strs ('0' : r0) = (NotSensible,r0)
  readRW strs ('1' : r0) = (PSensible,r0)
  readRW strs ('2' : r0) = (Sensible,r0)

  showRW params strs0 NotSensible = (strs0,showChar '0')
  showRW params strs0 PSensible = (strs0,showChar '1')
  showRW params strs0 Sensible = (strs0,showChar '2')

  writeRW params h NotSensible strs = hPutChar h '0' >> return strs
  writeRW params h PSensible strs = hPutChar h '1' >> return strs
  writeRW params h Sensible strs = hPutChar h '2' >> return strs

  typeOf _ = monoRWType "Sensible"

------------------------------------------------------------------------------
