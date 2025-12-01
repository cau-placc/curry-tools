------------------------------------------------------------------------------
--- This module contains operations to transform FlatCurry entities
--- into some normalized form so that they can be easier compared
--- for equivalence.
---
--- For instance, a type expression is normalized by replacing
--- type variable indices into a uniquely enumerated form.
---
--- @author Michael Hanus
--- @version May 2021
------------------------------------------------------------------------------

module FlatCurry.Normalize ( normalizeTypeExpr )
 where

import Control.Monad.Trans.State

import FlatCurry.Types

------------------------------------------------------------------------------
-- The state used during the normalization process.
-- The state consists of a current number for enumerating type variables
-- and a mapping from the original type variable indices into
-- normalized indices (which will be expanded during the transformation).
data TransInfo = TransInfo { currNr :: TVarIndex
                           , tvarMap :: [(TVarIndex,TVarIndex)]
                           }

-- The initial state.
initState :: TransInfo
initState = TransInfo 0 []

-- The type of the state normalization monad.
type TransState a = State TransInfo a


-- Auxiliary operation: get a unique index for a given type variable.
-- Either return the existing index or create a fresh one and update
-- the state.
getTVarIndex :: TVarIndex -> TransState TVarIndex
getTVarIndex v = do
  ti <- get
  maybe (do let nv = currNr ti
            put ti { currNr = nv + 1, tvarMap = (v,nv) : tvarMap ti }
            return nv )
        return
        (lookup v (tvarMap ti))

------------------------------------------------------------------------------

--- Normalize a type expression by enumerating the type variables
--- starting from `0`.
normalizeTypeExpr :: TypeExpr -> TypeExpr
normalizeTypeExpr texp = evalState (normTExp texp) initState

-- The actual implementation of the normalization task performs
-- a monadic traversal over the given type expression.
normTExp :: TypeExpr -> TransState TypeExpr
normTExp texp = case texp of
  TVar v -> do vi <- getTVarIndex v
               return $ TVar vi
  FuncType t1 t2 -> do
    nt1 <- normTExp t1
    nt2 <- normTExp t2
    return (FuncType nt1 nt2)
  TCons qn tes -> do
    ntes <- mapM normTExp tes
    return $ TCons qn ntes
  ForallType tvs te -> do
    ntvs <- mapM normTVarWithKind tvs
    nte  <- normTExp te
    return $ ForallType ntvs nte
 where
  normTVarWithKind (v,k) = do
    vi <- getTVarIndex v
    return (vi,k)

-----------------------------------------------------------------------
