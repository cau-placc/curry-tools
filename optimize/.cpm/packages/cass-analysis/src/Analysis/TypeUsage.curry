------------------------------------------------------------------------
--- Analysis of properties related to the usage and occurrences of types.
---
--- @author Michael Hanus
--- @version February 2017
------------------------------------------------------------------------

module Analysis.TypeUsage(showTypeNames,typesInValuesAnalysis) where

import Analysis.Types
import FlatCurry.Types
import Data.List       (intercalate)

------------------------------------------------------------------------
-- This analysis associates to each type the types which might occur
-- in values of this type. If a type occurs in its associated types,
-- it is a recursive type.

typesInValuesAnalysis :: Analysis [QName]
typesInValuesAnalysis =
  dependencyTypeAnalysis "TypesInValues" [] typesInTypeDecl

-- Show a list of type constructor names as a string.
showTypeNames :: AOutFormat -> [QName] -> String
showTypeNames _ tcs = intercalate ", " $ map (\ (mn,fn) -> mn ++ "." ++ fn) tcs

typesInTypeDecl :: TypeDecl -> [(QName,[QName])] -> [QName]

typesInTypeDecl (Type _ _ _ conDecls) usedtypes =
  foldr join [] $ map typesInConsDecl conDecls
 where
  typesInConsDecl (Cons _ _ _ typeExprs) =
    foldr join [] $ map (typesInTypeExpr usedtypes) typeExprs

typesInTypeDecl (TypeSyn _ _ _ typeExpr) usedtypes =
  typesInTypeExpr usedtypes typeExpr

typesInTypeDecl (TypeNew _ _ _ (NewCons _ _ typeExpr)) usedtypes =
  typesInTypeExpr usedtypes typeExpr


-- Computes all type constructors occurring in a type expression.
typesInTypeExpr :: [(QName,[QName])] -> TypeExpr -> [QName]
typesInTypeExpr _ (TVar _) = []
typesInTypeExpr usedtypes (FuncType t1 t2) =
  join (typesInTypeExpr usedtypes t1) (typesInTypeExpr usedtypes t2)
typesInTypeExpr usedtypes (TCons tc texps) =
  foldr join (join [tc] (maybe [] id (lookup tc usedtypes)))
             (map (typesInTypeExpr usedtypes) texps)
typesInTypeExpr usedtypes (ForallType _ t) = typesInTypeExpr usedtypes t

join :: [QName] -> [QName] -> [QName]
join tcs1 tcs2 = foldr insert tcs2 tcs1
 where
  insert x []     = [x]
  insert x (y:ys) | x == y    = y : ys
                  | x < y     = x : y : ys
                  | otherwise = y : insert x ys

-----------------------------------------------------------------------
