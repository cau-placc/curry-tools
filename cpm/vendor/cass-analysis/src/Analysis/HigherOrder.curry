------------------------------------------------------------------------
--- Analysis of higher-order properties of types and operations.
------------------------------------------------------------------------

module Analysis.HigherOrder
  (Order(..),showOrder,hiOrdType,hiOrdCons,hiOrdFunc) where

import Analysis.Types
import Analysis.ProgInfo
import FlatCurry.Types
import FlatCurry.Goodies
import Data.Maybe

-- datatype order: higher-order or first-order
data Order = HO | FO
  deriving (Show, Read, Eq)

-- Show higher-order information as a string.
showOrder :: AOutFormat -> Order -> String
showOrder _ HO = "higher-order"
showOrder _ FO = "first-order"

hoOr :: Order -> Order -> Order
hoOr HO _ = HO
hoOr FO x = x

------------------------------------------------------------------------
-- higher-order data type analysis

hiOrdType :: Analysis Order
hiOrdType =  dependencyTypeAnalysis "HiOrderType" FO orderOfType

orderOfType :: TypeDecl -> [(QName,Order)] -> Order

orderOfType (Type _ _ _ conDecls) usedtypes =
 hoOr (foldr hoOr FO (map orderOfConsDecl conDecls))
      (foldr hoOr FO (map snd usedtypes))
 where
   orderOfConsDecl (Cons _ _ _ typeExprs) =
     foldr hoOr FO (map orderOfTypeExpr typeExprs)

orderOfType (TypeSyn _ _ _ typeExpr) usedtypes =
 hoOr (orderOfTypeExpr typeExpr) (foldr hoOr FO (map snd usedtypes))

orderOfType (TypeNew _ _ _ (NewCons _ _ typeExpr)) usedtypes =
 hoOr (orderOfTypeExpr typeExpr) (foldr hoOr FO (map snd usedtypes))


-- compute the order of a type expression (ignore the type constructors,
-- i.e., check whether this expression contains a `FuncType`).
orderOfTypeExpr :: TypeExpr -> Order
orderOfTypeExpr (TVar _) = FO
orderOfTypeExpr (FuncType _ _) = HO
orderOfTypeExpr (TCons _ typeExprs) =
  foldr hoOr FO (map orderOfTypeExpr typeExprs)
orderOfTypeExpr (ForallType _ texp) = orderOfTypeExpr texp

-----------------------------------------------------------------------
-- higher-order constructor analysis

hiOrdCons :: Analysis Order
hiOrdCons =  simpleConstructorAnalysis "HiOrderConstr" orderOfConsDecl
 where
   orderOfConsDecl (Cons _ _ _ typeExprs) _ =
     foldr hoOr FO (map orderOfTypeExpr typeExprs)


-----------------------------------------------------------------------
-- higher-order function analysis

hiOrdFunc :: Analysis Order
hiOrdFunc = combinedSimpleFuncAnalysis "HiOrderFunc" hiOrdType orderOfFunc

orderOfFunc :: ProgInfo Order -> FuncDecl-> Order
orderOfFunc orderMap func =
  orderOfFuncTypeArity orderMap (funcType func) (funcArity func)

orderOfFuncTypeArity :: ProgInfo Order -> TypeExpr -> Int -> Order
orderOfFuncTypeArity orderMap functype arity =
  if arity==0
  then
   case functype of
     FuncType _ _   -> HO
     TVar (-42)     -> HO
     TCons x (y:ys) -> hoOr (orderOfFuncTypeArity orderMap y 0)
                            (orderOfFuncTypeArity orderMap (TCons x ys) 0)
     TCons tc [] -> fromMaybe FO (lookupProgInfo tc orderMap)
     _ -> FO
  else let (FuncType x y) = functype
        in hoOr (orderOfFuncTypeArity orderMap x 0)
                (orderOfFuncTypeArity orderMap y (arity-1))

-----------------------------------------------------------------------
