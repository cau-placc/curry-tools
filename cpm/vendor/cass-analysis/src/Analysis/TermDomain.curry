------------------------------------------------------------------------------
--- This module defines a class `TermDomain` which collects operations
--- on an abstract domain approximating sets of data terms
--- to be used in program analyses.
--- Furthermore, it defines instances for a domain of top-level constructors
--- and domains of depth-bounded constructor terms.
---
--- @author Michael Hanus
--- @version November 2024
------------------------------------------------------------------------------

{-# OPTIONS_FRONTEND -Wno-incomplete-patterns #-}

module Analysis.TermDomain
  ( TermDomain(..), AType, DType2, DType5, litAsCons )
 where

import Data.List ( intercalate )
import System.IO
import Test.Prop

import FlatCurry.Types
import RW.Base

------------------------------------------------------------------------------
--- The class `TermDomain` contains operations necessary to implement
--- program analyses related to abstract domains approximating sets of
--- data terms.
--- The additional class contexts are required since abstract domains
--- have to be stored, read and compared for equality in fixpoint operations.
class (Read a, Show a, Eq a, ReadWrite a) => TermDomain a where
  --- Abstract representation of no possible value.
  emptyType :: a
  --- Does an abstract type represent no value?
  isEmptyType :: a -> Bool
  --- Abstract representation of the type of all values.
  anyType :: a
  --- Does an abstract type represent any value?
  isAnyType :: a -> Bool
  --- The representation of a constructor application to a list of
  --- abstract argument types.
  aCons :: QName -> [a] -> a
  -- The representation of a literal in the abstract type domain.
  aLit :: Literal -> a
  --- The list of top-level constructors covered by an abstract type.
  --- The list is empty for `anyType`.
  consOfType :: a -> [QName]
  --- The argument types of an abstract type (given as the last argument)
  --- when it matches a given constructor/arity.
  argTypesOfCons :: QName -> Int -> a -> [a]
  --- Least upper bound of abstract values.
  lubType :: a -> a -> a
  --- Join two abstract values.
  --- The result is `emptyType` if they are not compatible.
  joinType :: a -> a -> a
  --- Shows an abstract value.
  showType :: a -> String

--- A literal `l` is represented as a constructor `("",l)`.
litAsCons :: Literal -> QName
litAsCons l = ("", showLit l)
 where
  showLit (Intc i)   = show i
  showLit (Floatc x) = show x
  showLit (Charc c)  = show c

------------------------------------------------------------------------------
--- An abstract term domain where terms are abstracted into their
--- top-level constructors.
--- `AAny` represents any expression, and
--- `ACons cs` a value rooted by some of the constructor `cs`.
---
--- An invariant (ensured by the interface operations):
--- the constructors in the `ACons` argument are always ordered.
data AType = ACons [QName] | AAny
 deriving (Eq, Show, Read)

isEmptyAType :: AType -> Bool
isEmptyAType AAny       = False
isEmptyAType (ACons cs) = null cs

isAnyAType :: AType -> Bool
isAnyAType AAny      = True
isAnyAType (ACons _) = False

--- Least upper bound of abstract values.
lubAType :: AType -> AType -> AType
lubAType AAny       _         = AAny
lubAType (ACons _) AAny       = AAny
lubAType (ACons c) (ACons d) = ACons (union c d)
 where
  -- Union on sorted lists:
  union []       ys     = ys
  union xs@(_:_) []     = xs
  union (x:xs)   (y:ys) | x==y      = x : union xs ys
                        | x<y       = x : union xs (y:ys)
                        | otherwise = y : union (x:xs) ys

--- Join two abstract values.
--- The result is `emptyType` if they are not compatible.
joinAType :: AType -> AType -> AType
joinAType AAny       av        = av
joinAType (ACons c) AAny       = ACons c
joinAType (ACons c) (ACons d) = ACons (intersect c d)
 where
  -- Intersection on sorted lists:
  intersect []     _      = []
  intersect (_:_)  []     = []
  intersect (x:xs) (y:ys) | x==y      = x : intersect xs ys
                          | x<y       = intersect xs (y:ys)
                          | otherwise = intersect (x:xs) ys

-- Shows an abstract value.
showAType :: AType -> String
showAType AAny        = "_"
showAType (ACons cs) = "{" ++ intercalate "," (map snd cs) ++ "}"

--- The `AType` instance of `TermDomain`.
instance TermDomain AType where
  emptyType             = ACons []
  isEmptyType           = isEmptyAType
  anyType               = AAny
  isAnyType             = isAnyAType
  aCons qc _            = ACons [qc]
  aLit l                = aCons (litAsCons l) []
  consOfType AAny       = []
  consOfType (ACons cs) = cs
  argTypesOfCons _ ar _ = take ar (repeat anyType)
  lubType               = lubAType
  joinType              = joinAType
  showType              = showAType

------------------------------------------------------------------------------
--- An abstract term domain of depth-bounded terms, i.e.,
--- constructor terms where an argument of a constructor
--- is either a depth-bounded term or `DAny`.
---
--- An invariant (ensured by the interface operations):
--- the constructors in the `DCons` argument are always ordered.
data DType = DCons [(QName, [DType])] | DAny
 deriving (Eq, Show, Read)

--- Cut a depth-k term at all branches larger than a given depth.
cutDType :: Int -> DType -> DType
cutDType _ DAny = DAny
cutDType d (DCons cs)
  | d==0      = DAny
  | otherwise = DCons (map (\ (c,args) -> (c, map (cutDType (d-1)) args)) cs)

--- Abstract representation of no possible value.
emptyDType :: DType
emptyDType = DCons []

isEmptyDType :: DType -> Bool
isEmptyDType DAny       = False
isEmptyDType (DCons cs) = null cs

--- Abstract representation of the type of all values.
anyDType :: DType
anyDType = DAny

isAnyDType :: DType -> Bool
isAnyDType DAny      = True
isAnyDType (DCons _) = False

--- Abstract representation of single constructor with abstract arguments.
--- The first argument is the depth bound of the terms.
dCons :: Int -> QName -> [DType] -> DType
dCons depthk qc ts = cutDType depthk (DCons [(qc,ts)])

--- The list of top-level constructors covered by an abstract type.
--- The list is empty for `anyDType`.
consOfDType :: DType -> [QName]
consOfDType DAny       = []
consOfDType (DCons cs) = map fst cs

--- The argument types of an abstract type (given as the last argument)
--- when it matches a given constructor/arity.
argDTypesOfCons :: QName -> Int -> DType -> [DType]
argDTypesOfCons _  ar DAny       = take ar (repeat anyDType)
argDTypesOfCons qn ar (DCons cs) =
  maybe (take ar (repeat emptyDType)) -- no matching constructor
        id
        (lookup qn cs)

--- Least upper bound of abstract values.
lubDType :: DType -> DType -> DType
lubDType DAny       _          = DAny
lubDType (DCons _)  DAny       = DAny
lubDType (DCons cs) (DCons ds) = DCons (union cs ds)
 where
  union []       ys     = ys
  union xs@(_:_) []     = xs
  union ((c,cts):xs) ((d,dts):ys)
    | c==d      = (c, map (uncurry lubDType) (zip cts dts)) : union xs ys
    | c<d       = (c,cts) : union xs ((d,dts):ys)
    | otherwise = (d,dts) : union ((c,cts):xs) ys

--- Join two abstract values.
--- The result is `emptyDType` if they are not compatible.
joinDType :: DType -> DType -> DType
joinDType DAny       av         = av
joinDType (DCons cs) DAny       = DCons cs
joinDType (DCons cs) (DCons ds) = DCons (intersect cs ds)
 where
  intersect []     _      = []
  intersect (_:_)  []     = []
  intersect ((c,cts):xs) ((d,dts):ys)
    | c==d      = let cdts = map (uncurry joinDType) (zip cts dts)
                  in if any (== emptyDType) cdts then intersect xs ys
                                                 else (c,cdts) : intersect xs ys
    | c<d       = intersect xs ((d,dts):ys)
    | otherwise = intersect ((c,cts):xs) ys

-- Shows an abstract value.
showDType :: DType -> String
showDType = showDT False
 where
  showDT _   DAny       = "_"
  showDT brkt (DCons cs) = case cs of
    []            -> "{}"
    [(c,[])]      -> snd c
    [(c,[t1,t2])] -> if any isAlphaNum (snd c) -- not an infix operator?
                       then showStd c [t1,t2]
                       else bracketIf brkt $
                              showDT True t1 ++ snd c ++ showDT True t2
    [(c,ts)]      -> showStd c ts
    _             ->
      "{" ++ intercalate ", " (map (\c -> showDT False (DCons [c])) cs) ++ "}"
   where
    showStd c ts =
      bracketIf brkt (intercalate " " (snd c : map (showDT True) ts))

    bracketIf b s = if b then "(" ++ s ++ ")" else s

--- The `DType` instance of `TermDomain` for depth 2.
data DType2 = DT2 DType
 deriving (Eq, Show, Read)

instance TermDomain DType2 where
  emptyType           = DT2 emptyDType
  isEmptyType (DT2 t) = isEmptyDType t
  anyType             = DT2 anyDType
  isAnyType (DT2 t)   = isAnyDType t
  aCons qc ts         = DT2 (dCons 2 qc (map (\ (DT2 t) -> t) ts))
  aLit l              = DT2 (dCons 2 (litAsCons l) [])
  consOfType (DT2 t)  = consOfDType t
  argTypesOfCons qn i (DT2 t) = map DT2 (argDTypesOfCons qn i t)
  lubType  (DT2 t1) (DT2 t2) = DT2 (lubDType t1 t2)
  joinType (DT2 t1) (DT2 t2) = DT2 (joinDType t1 t2)
  showType (DT2 dt) = showDType dt

--- The `DType` instance of `TermDomain` for depth 5.
data DType5 = DT5 DType
 deriving (Eq, Show, Read)

instance TermDomain DType5 where
  emptyType           = DT5 emptyDType
  isEmptyType (DT5 t) = isEmptyDType t
  anyType             = DT5 anyDType
  isAnyType (DT5 t)   = isAnyDType t
  aCons qc ts         = DT5 (dCons 5 qc (map (\ (DT5 t) -> t) ts))
  aLit l              = DT5 (dCons 5 (litAsCons l) [])
  consOfType (DT5 t)  = consOfDType t
  argTypesOfCons qn i (DT5 t) = map DT5 (argDTypesOfCons qn i t)
  lubType  (DT5 t1) (DT5 t2) = DT5 (lubDType t1 t2)
  joinType (DT5 t1) (DT5 t2) = DT5 (joinDType t1 t2)
  showType (DT5 dt) = showDType dt

------------------------------------------------------------------------------
-- Testing:

{-
pre :: String -> QName
pre n = ("Prelude",n)

aTrue, aFalse, aFalseTrue, aNothing, aJustTrue, aJustFalse :: DType
aTrue  = dCons 2 (pre "True") []
aFalse = dCons 2 (pre "False") []
aFalseTrue = DCons [(pre "False", []), (pre "True", [])]
aNothing   = dCons 2 (pre "Nothing") []
aJustTrue  = dCons 2 (pre "Just") [aTrue]
aJustFalse = dCons 2 (pre "Just") [aFalse]

cutDType'test1, cutDType'test2, cutDType'test3 :: Prop
cutDType'test1 = cutDType 0 aTrue -=- anyDType
cutDType'test2 = cutDType 1 aTrue -=- aTrue
cutDType'test3 = cutDType 1 aJustTrue -=- DCons [(pre "Just",[DAny])]

lub'test1, lub'test2, join'test1, join'test2 :: Prop
lub'test1 = lubDType aTrue aTrue -=- aTrue
lub'test2 = lubDType aTrue aFalse -=- aFalseTrue
join'test1 = joinDType aJustTrue aJustFalse -=- emptyDType
join'test2 = joinDType aJustTrue aJustTrue  -=- aJustTrue
-}

------------------------------------------------------------------------------
--- Definition of ReadWrite instance for compact data representation.

instance ReadWrite AType where
  readRW strs ('0' : r0) = (ACons a',r1)
    where
      (a',r1) = readRW strs r0
  readRW _ ('1' : r0) = (AAny,r0)

  showRW params strs0 (ACons a') = (strs1,showChar '0' . show1)
    where
      (strs1,show1) = showRW params strs0 a'
  showRW _ strs0 AAny = (strs0,showChar '1')

  writeRW params h (ACons a') strs =
    hPutChar h '0' >> writeRW params h a' strs
  writeRW _ h AAny strs = hPutChar h '1' >> return strs

  typeOf _ = monoRWType "AType"

instance ReadWrite DType where
  readRW strs ('0' : r0) = (DCons a',r1)
    where
      (a',r1) = readRW strs r0
  readRW _ ('1' : r0) = (DAny,r0)

  showRW params strs0 (DCons a') = (strs1,showChar '0' . show1)
    where
      (strs1,show1) = showRW params strs0 a'
  showRW _ strs0 DAny = (strs0,showChar '1')

  writeRW params h (DCons a') strs =
    hPutChar h '0' >> writeRW params h a' strs
  writeRW _ h DAny strs = hPutChar h '1' >> return strs

  typeOf _ = monoRWType "DType"

instance ReadWrite DType2 where
  readRW strs r0 = (DT2 a',r1)
    where
      (a',r1) = readRW strs r0

  showRW params strs0 (DT2 a') = (strs1,show1)
    where
      (strs1,show1) = showRW params strs0 a'

  writeRW params h (DT2 a') strs = writeRW params h a' strs

  typeOf _ = monoRWType "DType2"

instance ReadWrite DType5 where
  readRW strs r0 = (DT5 a',r1)
    where
      (a',r1) = readRW strs r0

  showRW params strs0 (DT5 a') = (strs1,show1)
    where
      (strs1,show1) = showRW params strs0 a'

  writeRW params h (DT5 a') strs = writeRW params h a' strs

  typeOf _ = monoRWType "DType5"

------------------------------------------------------------------------------
