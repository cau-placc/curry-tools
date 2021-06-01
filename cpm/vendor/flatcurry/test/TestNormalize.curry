--- Some tests for library `FlatCurry.Normalize`
---
--- To run all tests automatically by the currycheck tool, use the command:
--- `curry-check TestNormalize`
--- 
--- @author Michael Hanus

import FlatCurry.Types
import FlatCurry.Normalize
import Test.Prop

revType1 :: TypeExpr
revType1 =
  ForallType [(0,KStar)]
    (FuncType (TCons ("Prelude","[]") [TVar 0])
              (FuncType (TCons ("Prelude","[]") [TVar 0])
                        (TCons ("Prelude","[]") [TVar 0])))
revType2 :: TypeExpr
revType2 =
  ForallType [(1,KStar)]
    (FuncType (TCons ("Prelude","[]") [TVar 1])
              (FuncType (TCons ("Prelude","[]") [TVar 1])
                        (TCons ("Prelude","[]") [TVar 1])))

equivType :: Prop
equivType = normalizeTypeExpr revType1 -=- normalizeTypeExpr revType2
