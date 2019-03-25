{-# LANGUAGE MultiParamTypeClasses #-}
import Data.IORef
import System.IO.Unsafe   (unsafePerformIO) -- for global associations
import System.Process     (readProcessWithExitCode, runInteractiveCommand)
import Control.Concurrent (forkIO)
import System.IO

external_d_C_prim_execCmd :: Curry_Prelude.C_String -> Cover -> ConstStore
  -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple3 Curry_IO.C_Handle Curry_IO.C_Handle Curry_IO.C_Handle)
external_d_C_prim_execCmd str _ _ = toCurry
  (\s -> do (h1,h2,h3,_) <- runInteractiveCommand s
            return (OneHandle h1, OneHandle h2, OneHandle h3)) str

external_d_C_prim_evalCmd :: Curry_Prelude.C_String -> Curry_Prelude.OP_List Curry_Prelude.C_String -> Curry_Prelude.C_String
  -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple3 Curry_Prelude.C_Int Curry_Prelude.C_String Curry_Prelude.C_String)
external_d_C_prim_evalCmd cmd args input _ _
  = toCurry readProcessWithExitCode cmd args input

external_d_C_prim_connectToCmd :: Curry_Prelude.C_String -> Cover -> ConstStore
                               -> Curry_Prelude.C_IO Curry_IO.C_Handle
external_d_C_prim_connectToCmd str _ _ = toCurry
  (\s -> do (hin,hout,herr,_) <- runInteractiveCommand s
            forkIO (forwardError herr)
            return (InOutHandle hout hin)) str

forwardError :: Handle -> IO ()
forwardError h = do
   eof <- hIsEOF h
   if eof then return ()
          else hGetLine h >>= hPutStrLn stderr >> forwardError h


-----------------------------------------------------------------------
-- Implementation of global associations as simple association lists
-- (could be later improved by a more efficient implementation, e.g., maps)

type Assocs = [(String,String)]

assocs :: IORef Assocs
assocs = unsafePerformIO (newIORef [])

external_d_C_prim_setAssoc :: Curry_Prelude.C_String -> Curry_Prelude.C_String -> Cover -> ConstStore
                           -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
external_d_C_prim_setAssoc str1 str2 _ _ = toCurry
  (\key val -> do as <- readIORef assocs
                  writeIORef assocs ((key,val):as)) str1 str2

external_d_C_prim_getAssoc :: Curry_Prelude.C_String -> Cover -> ConstStore
                           -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe (Curry_Prelude.C_String))
external_d_C_prim_getAssoc str _ _ = toCurry
  (\key -> do as <- readIORef assocs
              return (lookup key as)) str

-----------------------------------------------------------------------
-- Implementation of IORefs in Curry. Note that we store Curry values
-- (and not the corresponding Haskell values) in the Haskell IORefs
data C_IORef a
    = Choice_C_IORef Cover ID (C_IORef a) (C_IORef a)
    | Choices_C_IORef Cover ID ([C_IORef a])
    | Fail_C_IORef Cover FailInfo
    | Guard_C_IORef Cover  Constraints (C_IORef a)
    | C_IORef (IORef a)

instance Show (C_IORef a) where
  show = error "ERROR: no show for IORef"

instance Read (C_IORef a) where
  readsPrec = error "ERROR: no read for IORef"

instance NonDet (C_IORef a) where
  choiceCons = Choice_C_IORef
  choicesCons = Choices_C_IORef
  failCons = Fail_C_IORef
  guardCons = Guard_C_IORef
  try (Choice_C_IORef cd i x y) = tryChoice cd i x y
  try (Choices_C_IORef cd s xs) = tryChoices cd s xs
  try (Fail_C_IORef cd info) = Fail cd info
  try (Guard_C_IORef cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_IORef  cd i x y)                 = f cd i x y
  match _ f _ _ _ _ (Choices_C_IORef cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_IORef cd i@(FreeID _ _)     xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_IORef _ i _)      =
    error ("IOExts.IORef.match: Choices with ChoiceID " ++ show i)
  match _ _ _ f _ _ (Fail_C_IORef cd info)                     = f cd info
  match _ _ _ _ f _ (Guard_C_IORef cd cs e)                    = f cd cs e
  match _ _ _ _ _ f x                                          = f x

instance Generable (C_IORef a) where
  generate _ _ = error "ERROR: no generator for IORef"

instance NormalForm (C_IORef a) where
  ($!!) cont ioref@(C_IORef _)            cd cs = cont ioref cd cs
  ($!!) cont (Choice_C_IORef d i io1 io2) cd cs = nfChoice cont d i io1 io2 cd cs
  ($!!) cont (Choices_C_IORef d i ios)    cd cs = nfChoices cont d  i ios cd cs
  ($!!) cont (Guard_C_IORef d c io)       cd cs
    = guardCons d c ((cont $!! io) cd $! (addCs c cs))
  ($!!) _    (Fail_C_IORef d info)        _  _  = failCons d info
  ($##) cont io@(C_IORef _)               cd cs = cont io cd cs
  ($##) cont (Choice_C_IORef d i io1 io2) cd cs = gnfChoice cont d i io1 io2 cd cs
  ($##) cont (Choices_C_IORef d i ios)    cd cs = gnfChoices cont d i ios cd cs
  ($##) cont (Guard_C_IORef d c io)       cd cs
    = guardCons d c ((cont $## io) cd $! (addCs c cs))
  ($##) _    (Fail_C_IORef d info)        cd cs = failCons d info
  searchNF _ cont ioref@(C_IORef _)        = cont ioref

instance Unifiable (C_IORef a) where
  (=.=) _ _ = error "(=.=) for C_IORef"
  (=.<=) _ _ = error "(=.<=) for C_IORef"
  bind cd i (Choice_C_IORef d j l r) = [(ConstraintChoice d j (bind cd i l) (bind cd i r))]
  bind cd i (Choices_C_IORef d j@(FreeID _ _) xs) = bindOrNarrow cd i d j xs
  bind cd i (Choices_C_IORef d j@(NarrowedID _ _) xs) = [(ConstraintChoices d j (map (bind cd i) xs))]
  bind _  _ (Fail_C_IORef cd info) = [Unsolvable info]
  bind cd i (Guard_C_IORef _ cs e) = (getConstrList cs) ++ (bind cd i e)
  lazyBind cd i (Choice_C_IORef d j l r) = [(ConstraintChoice d j (lazyBind cd i l) (lazyBind cd i r))]
  lazyBind cd i (Choices_C_IORef d j@(FreeID _ _) xs) = lazyBindOrNarrow cd i d j xs
  lazyBind cd i (Choices_C_IORef d j@(NarrowedID _ _) xs) = [(ConstraintChoices d j (map (lazyBind cd i) xs))]
  lazyBind _  _ (Fail_C_IORef cd info) = [Unsolvable info]
  lazyBind cd i (Guard_C_IORef _ cs e) = (getConstrList cs) ++ [(i :=: (LazyBind (lazyBind cd i e)))]

instance Curry_Prelude.Curry a => Curry_Prelude.Curry (C_IORef a)

instance ConvertCurryHaskell (C_IORef a) (IORef a) where
  fromCurry (C_IORef r) = r
  fromCurry _           = error "IORef with no ground term occurred"
  toCurry r             = C_IORef r

external_d_C_newIORef :: Curry_Prelude.Curry a => a -> Cover -> ConstStore
                      -> Curry_Prelude.C_IO (C_IORef a)
external_d_C_newIORef cv _ _ = toCurry (newIORef cv)

external_d_C_prim_readIORef :: Curry_Prelude.Curry a => C_IORef a -> Cover -> ConstStore
                            -> Curry_Prelude.C_IO a
external_d_C_prim_readIORef ref _ _ = fromIO (readIORef (fromCurry ref))

external_d_C_prim_writeIORef :: Curry_Prelude.Curry a => C_IORef a -> a
                             -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
external_d_C_prim_writeIORef ref cv _ _ = toCurry (writeIORef (fromCurry ref) cv)
