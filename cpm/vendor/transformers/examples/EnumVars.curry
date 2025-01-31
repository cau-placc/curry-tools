------------------------------------------------------------------------------
-- Example for using the State monad:
--
-- Given: an arithmetic expression containing numbers, variables (strings), etc
-- Task: replace all variables in an expression by unique numbers
------------------------------------------------------------------------------

import Test.Prop -- for testing

import Control.Monad.Trans.State

-- The representation of arithmetic expressions is parameterized
-- over the representation of variables.
data Exp a = Num Int
           | Var a
           | BinOp String (Exp a) (Exp a)
 deriving (Show, Eq)

-- An example expression with strings as variables.
exp1 :: Exp String
exp1 = BinOp "+" (Var "x") (BinOp "*" (Var "y") (Var "x"))

-- The representation of the state used in the task.
-- The state consists of a current number for enumerating the variables
-- and a mapping from variable names to unique numbers (which will
-- be extended during the transformation).
data TransInfo = TransInfo { currNr :: Int
                           , varMap :: [(String,Int)]
                           }
  deriving (Show, Eq) -- only required for testing, see below

-- The initial state.
initState :: TransInfo
initState = TransInfo 0 []

-- The type of our actual state monad contains the transformation state.
type TransState a = State TransInfo a

-- Auxiliary operation: get a unique index for a given variable name.
-- Either return the existing index or create a fresh one and update
-- the state.
getVarIndex :: String -> TransState Int
getVarIndex v = do
  ti <- get
  maybe (do let freshnr = currNr ti + 1
            put ti { currNr = freshnr, varMap = (v,freshnr) : varMap ti }
            return freshnr )
        return
        (lookup v (varMap ti))

-- The actual implementation of the transformation task performs
-- a traversal over the given expression in a monadic way.
enumExp :: Exp String -> TransState (Exp Int)
enumExp exp = case exp of
  Num n -> return $ Num n
  Var v -> do vi <- getVarIndex v
              return $ Var vi
  BinOp o e1 e2 -> do
    te1 <- enumExp e1
    te2 <- enumExp e2
    return $ BinOp o te1 te2

-- The final transformation operation hides the use of the state monad.
transExp :: Exp String -> Exp Int
transExp exp = evalState (enumExp exp) initState

main1 :: Exp Int
main1 = transExp exp1

-- If one also wants to use the final state after the transformation,
-- use `runState` instead of `evalState`.
runTransExp :: Exp String -> (Exp Int, TransInfo)
runTransExp exp = runState (enumExp exp) initState

main2 :: (Exp Int, TransInfo)
main2 = runTransExp exp1

------------------------------------------------------------------------------
-- Tests:

texp1 :: Exp Int
texp1 = BinOp "+" (Var 1) (BinOp "*" (Var 2) (Var 1))

test1 :: Prop
test1 = main1 -=- texp1

test2 :: Prop
test2 = main2 -=- (texp1, TransInfo 2 [("y",2),("x",1)])

test3 :: Int -> Prop
test3 n = evalState (enumExp (Var "a")) (TransInfo n []) -=- (Var (n+1))

test4 :: Int -> Prop
test4 n = evalState (enumExp exp1) (TransInfo n []) -=-
          BinOp "+" (Var (n+1)) (BinOp "*" (Var (n+2)) (Var (n+1)))

------------------------------------------------------------------------------
