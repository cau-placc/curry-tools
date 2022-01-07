------------------------------------------------------------------------------
--- This module contains operations to eliminate definitions and uses
--- of `newtype` in a FlatCurry program.
--- 
--- If there is a declaration of the form
--- 
---     newtype NTYPE a1...an = NTCONS te
--- 
--- in a Curry program, the following transformations are preformed:
--- 
--- - Replace `newtype` declaration by `data` declaration.
--- - Every type application `(NTYPE t1...tn)` is replaced by
---   `{a1 |-> t1,..., an |-> tn}(te)`
---   except for occurrences in instance definitions, i.e., operations
---   named by `_inst#...`.
--- - A constructor application `(NTCONS e)` is replaced by `e`.
--- - A partial constructor application `(NTCONS)` is replaced by
---   `(Prelude.id)`.
--- - A case expresion `(f)case x of { NTCONS y -> e}` is replaced by
---   `{y |-> x}(e)`.
---
--- @author Michael Hanus
--- @version March 2021
------------------------------------------------------------------------------

module FlatCurry.ElimNewtype
  ( elimNewtypeInProg, elimNewtype )
 where

import Data.List         ( isPrefixOf )

import FlatCurry.Files   ( readFlatCurryInt )
import FlatCurry.Goodies ( progImports, progTypes )
import FlatCurry.Types

--- Eliminates all `newtype` definitions/uses in a FlatCurry program.
--- For this purpose, the interfaces of the imported modules are read
--- before performing the transformation.
elimNewtypeInProg :: Prog -> IO Prog
elimNewtypeInProg prog = do
  impints <- mapM readFlatCurryInt (progImports prog)
  return $ elimNewtype impints prog

--- Eliminates all `newtype` definitions/uses in a FlatCurry program.
--- The first argument are the interfaces of the imported modules.
elimNewtype :: [Prog] -> Prog -> Prog
elimNewtype impprogs prog@(Prog mname imps tdecls fdecls ops) =
  if null nti
    then prog
    else Prog mname imps (map replaceNewtypeDecl tdecls)
              (map (elimNewtypeInFunc nti) fdecls) ops
 where
  nti = newtypesOfProg (tdecls ++ concatMap progTypes impprogs)

replaceNewtypeDecl :: TypeDecl -> TypeDecl
replaceNewtypeDecl td = case td of
  TypeNew tc tvis tvs (NewCons ct cvis te)
    -> Type tc tvis tvs [Cons ct 1 cvis [te]]
  _ -> td

elimNewtypeInFunc :: [NewtypeInfo] -> FuncDecl -> FuncDecl
elimNewtypeInFunc _   fd@(Func _  _  _   _     (External _))    = fd
elimNewtypeInFunc nti fd@(Func qf ar vis ftype (Rule args rhs)) =
  if isClassInstanceOp qf
    then fd
    else Func qf ar vis (elimType ftype) (Rule args (elimExp rhs))
 where
  elimType te = case te of
    TVar _             -> te
    FuncType t1 t2     -> FuncType (elimType t1) (elimType t2)
    TCons tc tes       -> elimTCons tc (map elimType tes)
    ForallType tvs fte -> ForallType tvs (elimType fte)

  elimTCons tc tes =
    maybe (TCons tc tes)
          (\ (tvs,_,ntexp) -> substTVarsInTExp (zip tvs tes) ntexp)
          (lookup tc nti)

  elimExp exp = case exp of
    Var _         -> exp
    Lit _         -> exp
    Comb ct qn es -> elimComb ct qn (map elimExp es)
    Let bs e      -> Let (map (\ (v,be) -> (v, elimExp be)) bs) (elimExp e)
    Free vs e     -> Free vs (elimExp e)
    Or e1 e2      -> Or (elimExp e1) (elimExp e2)
    Case ct ce bs -> elimCase ct (elimExp ce)
                       (map (\ (Branch pt be) -> Branch pt (elimExp be)) bs)
    Typed e t     -> Typed (elimExp e) t

  elimComb ct qn es = case ct of
    ConsCall       | length es == 1 && isNewCons qn
      -> head es
    ConsPartCall 1 | null es && isNewCons qn
      -> Comb (FuncPartCall 1) ("Prelude","id") []
    _ -> Comb ct qn es

  elimCase ct ce bs = case bs of
    [Branch (Pattern qn [v]) be] | isNewCons qn
      -> case ce of Var cv -> substVarInExp v cv be
                    _      -> Let [(v,ce)] be
    _ -> Case ct ce bs

  isNewCons qn = qn `elem` map (\ (_,(_,nc,_)) -> nc) nti

--- Applies a type substitution (first argument) to a type expression.
substTVarsInTExp :: [(TVarIndex,TypeExpr)] -> TypeExpr -> TypeExpr
substTVarsInTExp tvtexps te = subst te
 where
  subst texp = case texp of
    TVar v             -> maybe texp id (lookup v tvtexps)
    FuncType t1 t2     -> FuncType (subst t1) (subst t2)
    TCons tc tes       -> TCons tc (map subst tes)
    ForallType tvs fte -> ForallType tvs (subst fte)

--- Replaces a variable by another variable in an expressions, i.e.,
--- `substVarInExp x y e = {x |-> y}(e)`.
substVarInExp :: VarIndex -> VarIndex -> Expr -> Expr
substVarInExp x y e0 = subst e0
 where
  subst exp = case exp of
    Var v         -> Var (if v == x then y else v)
    Lit _         -> exp
    Comb ct qn es -> Comb ct qn (map subst es)
    Let bs e      -> Let (map (\ (v,be) -> (v, subst be)) bs) (subst e)
    Free vs e     -> Free vs (subst e)
    Or e1 e2      -> Or (subst e1) (subst e2)
    Case ct ce bs -> Case ct (subst ce)
                          (map (\ (Branch pt be) -> Branch pt (subst be)) bs)
    Typed e t     -> Typed (subst e) t


type NewtypeInfo = (QName,([TVarIndex],QName,TypeExpr))

--- Extracts `newtype` definitions occurring in a FlatCurry program.
newtypesOfProg :: [TypeDecl] -> [NewtypeInfo]
newtypesOfProg = concatMap ntOfTypeDecl
 where
  ntOfTypeDecl (Type    _  _ _   _                ) = []
  ntOfTypeDecl (TypeSyn _  _ _   _                ) = []
  ntOfTypeDecl (TypeNew tc _ tvs (NewCons ct _ te)) =
    [(tc, (map fst tvs, ct, te))]

isNewtypeDecl :: TypeDecl -> Bool
isNewtypeDecl td = case td of TypeNew _ _ _ _ -> True
                              _               -> False

--- Is the operation a class instance operation?
isClassInstanceOp :: QName -> Bool
isClassInstanceOp (_,f) = "_inst#" `isPrefixOf` f

-----------------------------------------------------------------------
