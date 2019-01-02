--------------------------------------------------------------
-- ExpVarFunPats
--------------------------------------------------------------

import Test.Prop


data Peano = O | S Peano
  deriving (Eq,Show)

data Exp = Num Peano | Var VarName | Add Exp Exp | Mul Exp Exp
  deriving (Eq,Show)

data VarName = X1 | X2 | X3
  deriving (Eq,Show)

data Position = Lt | Rt

evalTo e = Add (Num O) e
         ? Add e (Num O)
         ? Mul (Num (S O)) e
         ? Mul e (Num (S O))

replace :: Exp -> [Position] -> Exp -> Exp
replace _         []    x = x
replace (Add l r) (Lt:p) x = Add (replace l p x) r
replace (Add l r) (Rt:p) x = Add l (replace r p x)
replace (Mul l r) (Lt:p) x = Mul (replace l p x) r
replace (Mul l r) (Rt:p) x = Mul l (replace r p x)

genExpWithVar :: Int -> Exp
genExpWithVar n = if n==0 then Add (Var X1) (Num O)
                          else Mul (Num (S O)) (genExpWithVar (n-1))

genExpWithVar' :: Int -> Exp
genExpWithVar' n = if n==0 then Add (Var X1) (Num O)
                          else Mul (genExpWithVar' (n-1)) (Num (S O))

-- return some variable occurring in an expression:
varInExp :: Exp -> VarName
varInExp exp | replace x y (Var v) == exp
             = v
 where x, y, v free

-- find a variable in an expression having 203 nodes
main1 :: VarName
main1 = varInExp (genExpWithVar' 100)

test_varInExp = varInExp (genExpWithVar' 100) -=- X1

----------------------------------------------------------------
-- Simplify
----------------------------------------------------------------

simplify :: Exp -> Exp
simplify exp | replace c p (evalTo x) == exp
             = replace c p x
  where c, p, x free

genExpWithMult1 :: Int -> Exp
genExpWithMult1 n = if n==0 then Mul (Num (S O)) (Var X1)
                            else Mul (Num (S (S O))) (genExpWithMult1 (n-1))

expSize :: Exp -> Int
expSize (Num _) = 1
expSize (Var _) = 1
expSize (Add e1 e2) = expSize e1 + expSize e2 + 1
expSize (Mul e1 e2) = expSize e1 + expSize e2 + 1

-- make a single simplification step in an expression having 4003 nodes
main2 :: Int
main2 = expSize (simplify (genExpWithMult1 2000))

test_simplify = expSize (simplify (genExpWithMult1 2000)) -=- 4001
