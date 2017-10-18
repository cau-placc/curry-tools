---------------------------------------------------------------------------
--- Deterministic operations are marked by wrapping the result type
--- of top-level operations with the type synonym `Prelude.DET`.
--- This module defines the operation `checkDetUse` which detects
--- unintended uses of this type synonym.
---
--- See example program `Examples/UsageErrors.curry` for some examples.
---
--- @author Michael Hanus
--- @version October 2016
---------------------------------------------------------------------------

module CheckDetUsage(containsDetOperations, checkDetUse) where

import AbstractCurry.Types
import AbstractCurry.Select

---------------------------------------------------------------------
--- Does a Curr program contains operations with DET annotations?
containsDetOperations :: CurryProg -> Bool
containsDetOperations (CurryProg _ _ _ _ _ _ fdecls _) =
  any (detInTopLevelType . typeOfQualType . funcType) fdecls
 where
  detInTopLevelType (CTVar _)        = False
  detInTopLevelType (CTCons _)       = False
  detInTopLevelType (CFuncType _ rt) = detInTopLevelType rt
  detInTopLevelType (CTApply tc _)   = tc == CTCons (pre "DET")

---------------------------------------------------------------------
--- Returns messages about unintended uses of type synonym `DET`
--- in a Curry program.
checkDetUse :: CurryProg -> [(QName,String)]
checkDetUse (CurryProg _ _ _ _ _ _ fdecls _) =
  concatMap (map showDetError . checkDetUseInFDecl) fdecls
 where
  showDetError qf = (qf, "wrong use of DET type synonym!")

checkDetUseInFDecl :: CFuncDecl -> [QName]
checkDetUseInFDecl (CFunc qn _ _ t rs) =
  if checkDetInTopLevelType (typeOfQualType t) || any detInRule rs
  then [qn]
  else []
checkDetUseInFDecl (CmtFunc _  qn ar vis t rs) =
  checkDetUseInFDecl (CFunc qn ar vis t rs)

checkDetInTopLevelType :: CTypeExpr -> Bool
checkDetInTopLevelType (CTVar _)     = False
checkDetInTopLevelType (CTCons _)    = False
checkDetInTopLevelType (CFuncType at rt) =
  detInTypeExpr at || checkDetInTopLevelType rt
checkDetInTopLevelType (CTApply _ ta) = detInTypeExpr ta

detInTypeExpr :: CTypeExpr -> Bool
detInTypeExpr (CTVar _) = False
detInTypeExpr (CTCons tc) =  tc == pre "DET"
detInTypeExpr (CFuncType at rt) = detInTypeExpr at || detInTypeExpr rt
detInTypeExpr (CTApply   tc ta) = detInTypeExpr tc || detInTypeExpr ta

detInRule :: CRule -> Bool
detInRule (CRule _ rhs) = detInRhs rhs

detInRhs :: CRhs -> Bool
detInRhs (CSimpleRhs e ldecls) = detInExp e || any detInLocalDecl ldecls
detInRhs (CGuardedRhs gs ldcls) = any detInGuard gs || any detInLocalDecl ldcls
 where detInGuard (e1,e2) = detInExp e1 || detInExp e2

detInLocalDecl :: CLocalDecl -> Bool
detInLocalDecl (CLocalFunc (CFunc _ _ _ t rs)) =
  detInTypeExpr (typeOfQualType t) || any detInRule rs
detInLocalDecl (CLocalFunc (CmtFunc _ _ _ _ t rs)) =
  detInTypeExpr (typeOfQualType t) || any detInRule rs
detInLocalDecl (CLocalPat _ rhs) = detInRhs rhs
detInLocalDecl (CLocalVars _) = False

detInExp :: CExpr -> Bool
detInExp (CVar _) = False
detInExp (CLit _) = False
detInExp (CSymbol _) = False
detInExp (CApply e1 e2) = detInExp e1 || detInExp e2
detInExp (CLambda _ e) = detInExp e
detInExp (CLetDecl ldecls e) = any detInLocalDecl ldecls || detInExp e
detInExp (CDoExpr stmts) = any detInStatement stmts
detInExp (CListComp e stmts) = detInExp e || any detInStatement stmts
detInExp (CCase _ e branches) = detInExp e || any (detInRhs . snd) branches
detInExp (CTyped e t) = detInExp e || detInTypeExpr (typeOfQualType t)
detInExp (CRecConstr _ fields) = any (detInExp . snd) fields
detInExp (CRecUpdate e fields) = detInExp e || any (detInExp . snd) fields

detInStatement :: CStatement -> Bool
detInStatement (CSExpr e) = detInExp e
detInStatement (CSPat _ e) = detInExp e
detInStatement (CSLet ldecls) = any detInLocalDecl ldecls

---------------------------------------------------------------------
