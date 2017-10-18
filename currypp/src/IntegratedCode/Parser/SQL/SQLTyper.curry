--- Module for checking the type integrity of SQL expressions.
--- This includes ensuring the same type on both sides of (binary)
--- operations and in case expressions, correct types in insert 
--- and update statements and in all occasions in which at least one column 
--- reference is given. Furthermore foreign keys are marked for a
--- proper translation in later modules. In addition it is ensured
--- that no key columns can be updated directly,
--- that aggregation functions are called with the correct type,  
--- embedded expressions are not compared with each other
--- and null-values can't be used in case-expression.
--- Warnings are generated in case the type of an embedded expression
--- was set using context information.
--- The module is organized in two stages. The first one retrieves 
--- all kind of information about the used types from the parser info module.
--- The second stage checks the correct usage.
--- of the types.
--- @author: Julia Krone
--- @version: 0.1
-- --------------------------------------------------------------------------

module SQLTyper(checkTypes) where

import Char(toLower, toUpper)
import FiniteMap
import List(isInfixOf)
import Time

import ParseTypes

import SQLAst
import SQLParserInfoType

---Type Checker.
--- Organized in two stages:
--- 1. retrieving types from parser info
--- 2. check type integrity of expressions
--- If the first stage fails the second one is not started.
--- First stage is only started if a valid AST without errors is passed.
checkTypes :: PM [Statement] -> AttrTypeFM -> Pos -> PM [Statement]
checkTypes (WM (Errors err) ws) _ _ = WM (throwPR err) ws
checkTypes (WM (OK ast) ws) types p = 
  let (WM resPR warns) = sequencePM (map (retrieveTypes p types) ast)
   in case resPR of
           (Errors err) -> (WM (throwPR err) (ws++warns))
           (OK ast1)    -> let (WM resPR1 warns1) = 
                                       sequencePM (map (checkTypCons p) ast1)
                            in (WM resPR1 (warns1++warns++ws))

-- --------------------------------first stage -------------------------------

retrieveTypes :: Pos -> AttrTypeFM -> Statement -> PM Statement
retrieveTypes _ _ Rollback    = cleanPM Rollback
retrieveTypes _ _ Commit      = cleanPM Commit
retrieveTypes _ _ Transaction = cleanPM Transaction
retrieveTypes p types (InTransaction stats) =
   liftPM (\typedStats -> InTransaction typedStats)
          (sequencePM (map (retrieveTypes p types) stats))
retrieveTypes p types (Delete tab cond) =
   liftPM (\typedCond -> (Delete tab typedCond))
          (typCondition p types cond)
retrieveTypes p types (Insert tab cols valss) =
   liftPM (\typedCols -> Insert tab typedCols valss)
          (typInsertColumns p types cols)        
retrieveTypes _ _ st@(UpdateEntity _ _) = cleanPM st
retrieveTypes p types (Update tab assigns cond) =
   combinePMs (\typedAssigns typedCond -> (Update tab typedAssigns typedCond))
              (typAssignments p types assigns)
              (typCondition p types cond)
retrieveTypes p types (Select selHead order limit) =
   combinePMs (\typedHead typedOrd -> (Select typedHead typedOrd limit))
              (typSelHead p types selHead)
              (typOrder p types order)

-- ---------------------------- select - first stage -------------------------

typSelHead :: Pos -> AttrTypeFM -> SelectHead -> PM SelectHead
typSelHead p types (Query selClause tab cond gr) = 
             combinePMs (\(tSC,tTab) (tCond, tGr) -> (Query tSC tTab tCond tGr))
                        (combinePMs (,)
                                    (typSelClause p types selClause)
                                    (typTableRef p types tab))
                        (combinePMs (,)
                                    (typCondition p types cond)
                                    (typGroup p types gr))
typSelHead p types (Set setOp head1 head2) =
              combinePMs (\typHead1 typHead2 -> (Set setOp typHead1 typHead2))
                         (typSelHead p types head1)
                         (typSelHead p types head2)

typSelClause :: Pos -> AttrTypeFM -> SelectClause -> PM SelectClause
typSelClause _ _ sc@(SelAll _) = cleanPM sc
typSelClause p types (SelColumns sp elems) =
           liftPM (\typedElems -> (SelColumns sp typedElems))
                  (sequencePM (map (typSelElement p types) elems))

typSelElement :: Pos -> AttrTypeFM -> SelElement -> PM SelElement
typSelElement p types (Col col) = liftPM (\typedCol -> (Col typedCol))
                                         (typColumnRef p types col)
typSelElement p types (Case cond op1 op2) =
               combinePMs (\tCond (top1, top2) -> (Case tCond top1 top2))
                          (typCondition p types cond)
                          (combinePMs (,) (typOperand p types op1)
                                          (typOperand p types op2))
typSelElement p types (Aggregation fun sp col) =
               liftPM (\typedCol -> (Aggregation fun sp typedCol))
                      (typColumnRef p types col)

typTableRef :: Pos -> AttrTypeFM -> TableRef -> PM TableRef
typTableRef _ _ (TableRef tab Nothing) = cleanPM (TableRef tab Nothing)
typTableRef p types (TableRef tab (Just join)) = 
      liftPM (\tJoin -> (TableRef tab (Just tJoin)))
             (typJoinClause p types join)

typJoinClause :: Pos -> AttrTypeFM -> JoinClause -> PM JoinClause
typJoinClause _ _ jc@(CrossJoin _ Nothing) = cleanPM jc
typJoinClause p types (CrossJoin tab (Just join)) = 
    liftPM (\tJoin -> (CrossJoin tab (Just tJoin))) 
           (typJoinClause p types join)
typJoinClause p types (InnerJoin tab jCond Nothing) = 
    liftPM (\tJoinCond -> (InnerJoin tab tJoinCond Nothing))
           (typJoinCond p types jCond)
typJoinClause p types (InnerJoin tab jCond (Just join)) =
   combinePMs (\tCond tJoin -> (InnerJoin tab tCond (Just tJoin)))
              (typJoinCond p types jCond)
              (typJoinClause p types join)

typJoinCond :: Pos -> AttrTypeFM -> JoinCond -> PM JoinCond
typJoinCond p types (JC cond) = liftPM (\tcond -> (JC tcond))
                                       (typCondition p types cond)

typGroup :: Pos -> AttrTypeFM -> (Maybe Group) -> PM (Maybe Group)
typGroup _ _ Nothing = cleanPM Nothing
typGroup p types (Just (GroupBy cols hav)) =
            combinePMs (\tCols tHav -> (Just (GroupBy tCols tHav)))
                       (sequencePM (map (typColumnRef p types) cols))
                       (typHaving p types hav)

typHaving :: Pos -> AttrTypeFM -> Having -> PM Having
typHaving _ _ NoHave = cleanPM NoHave
typHaving p types (CmpHave logOp hav1 hav2) =
               combinePMs (\thav1 thav2 -> (CmpHave logOp thav1 thav2))
                          (typHaving p types hav1)
                          (typHaving p types hav2)
typHaving p types (AggrHave fun sp col op operand) =
                combinePMs (\tCol tOp -> (AggrHave fun sp tCol op tOp))
                           (typColumnRef p types col)
                           (typOperand p types operand)
typHaving p types (Neg hav) = liftPM (\typedHave -> Neg typedHave)
                                     (typHaving p types hav)
typHaving p types (SimpleHave cond) = 
        liftPM (\typedCond -> SimpleHave typedCond)
               (typCondition p types cond)

typOrder :: Pos -> AttrTypeFM -> Order -> PM Order
typOrder p types (OrderBy colDirs) = 
     liftPM (\tColDirs -> OrderBy tColDirs)
            (sequencePM (map (typSingleColDir p types) colDirs))

typSingleColDir :: Pos -> AttrTypeFM -> (ColumnRef, Dir) 
                                           -> PM (ColumnRef, Dir)
typSingleColDir p types (col, dir) = liftPM (\typedCol -> (typedCol, dir))
                                            (typColumnRef p types col)

-- ---------------------------- update - first stage --------------------------

typAssignments :: Pos -> AttrTypeFM -> [Assign] -> PM [Assign]
typAssignments p types assigns = 
        sequencePM (map (typAssignment p types) assigns)

typAssignment :: Pos -> AttrTypeFM -> Assign -> PM Assign
typAssignment p types (Assign col val) = 
     liftPM (\typedCol -> Assign typedCol val)
            (typColumnRef p types col)

-- ---------------------------- insert - first stage -------------------------

typInsertColumns :: Pos -> AttrTypeFM -> [ColumnRef] -> PM [ColumnRef]
typInsertColumns p types cols = sequencePM (map (typColumnRef p types) cols)

-- ----------------------------common elements - first stage ------------------

typCondition :: Pos -> AttrTypeFM -> Condition -> PM Condition
typCondition _ _ c@(FK _ _ _) = cleanPM c
typCondition p types (Cmp logOp cond1 cond2) =
   combinePMs (\ tc1 tc2 -> Cmp logOp tc1 tc2)
              (typCondition p types cond1)
              (typCondition p types cond2)
typCondition p types (Not cond) = liftPM (\tc -> Not tc)
                                         (typCondition p types cond)
typCondition p types (Exists stat) = liftPM (\tst -> Exists tst)
                                            (retrieveTypes p types stat)
typCondition p types (IsNull operand) = liftPM (\top -> IsNull top)
                                               (typOperand p types operand)
typCondition p types (NotNull operand) = liftPM (\top -> NotNull top)
                                                (typOperand p types operand)
typCondition p types (AIn op vals) = liftPM (\top -> (AIn top vals))
                                            (typOperand p types op)
typCondition p types (ABinOp binOp op1 op2) = 
       combinePMs (\ top1 top2 -> ABinOp binOp top1 top2)
                  (typOperand p types op1)
                  (typOperand p types op2)
typCondition p types (ABetween op1 op2 op3) =
       combinePMs (\top1 (top2, top3) -> (ABetween top1 top2 top3))
                  (typOperand p types op1)
                  (combinePMs (,) (typOperand p types op2)
                                  (typOperand p types op3))
typCondition _ _ NoCond = cleanPM NoCond

typOperand :: Pos -> AttrTypeFM -> Operand -> PM Operand
typOperand p types (Left col) = liftPM (\typedCol -> (Left typedCol))
                                       (typColumnRef p types col) 
typOperand _ _ (Right val) = cleanPM (Right val)

-- Core function of the first stage. Retrives type information
-- from parser info and inserts it into the column node.
typColumnRef :: Pos -> AttrTypeFM -> ColumnRef -> PM ColumnRef
typColumnRef p types (Column (Unique tab) col _ nl al) = 
  let typ = lookupFM types ((firstLow tab)++(firstUp col)) 
  in case typ of
       Nothing -> throwPM p ("Undefined type given for column "++tab++col) 
       (Just typeStr) ->
          case typeStr of
             "int"       -> cleanPM (Column (Unique tab) col I nl al)
             "float"     -> cleanPM (Column (Unique tab) col F nl al)
             "string"    -> cleanPM (Column (Unique tab) col S nl al)
             "char"      -> cleanPM (Column (Unique tab) col C nl al)
             "date"      -> cleanPM (Column (Unique tab) col D nl al)
             "bool"      -> cleanPM (Column (Unique tab) col B nl al)
             _           -> cleanPM (Column (Unique tab) col (Key typeStr) nl al)
-- this should not happen at this stage anymore.
typColumnRef p _ (Column (Def _) col _ _ _) =
  throwPM p ("Column "++col++" could not be related to any table.")

-- ---------------------------second stage ---------------------------------------

checkTypCons :: Pos -> Statement -> PM Statement
checkTypCons _ Rollback = cleanPM Rollback
checkTypCons _ Commit   = cleanPM Commit
checkTypCons _ Transaction = cleanPM Transaction
checkTypCons p (InTransaction stats) = 
   liftPM (\checkedStats -> InTransaction checkedStats)
          (sequencePM (map (checkTypCons p) stats))
checkTypCons p (Delete tab cond) =
   liftPM (\checkedCond -> (Delete tab checkedCond))
          (checkCondition p cond)
checkTypCons p (Insert tab cols valss) = 
          liftPM (\chValss -> (Insert tab cols chValss))
                 (checkInsertVals p tab cols valss)
checkTypCons p (UpdateEntity tab val) =
          liftPM (\chVal -> (UpdateEntity tab chVal))
                 (checkEntUpdate p tab val)
checkTypCons p (Update tab assigns cond) = 
          combinePMs (\chAssigns chCond -> (Update tab chAssigns chCond))
                     (checkAssignments p assigns)
                     (checkCondition p cond)
checkTypCons p (Select selHead order limit) = 
          liftPM (\chSelHead  -> (Select chSelHead order limit))
                 (checkSelHead p selHead)
                     

-- -------------------------- select - second stage ------------------------------

-- Checks the types in SelectHead-node, in particular throws an error if
-- types in parts of a combined select statement are not the same.
checkSelHead :: Pos -> SelectHead -> PM SelectHead
checkSelHead p (Query selClause tab cond gr) =
  combinePMs (\(chSelClause, chTab) (chCond, chGr) -> 
                                     (Query chSelClause chTab chCond chGr))
             (combinePMs (,) (checkSelClause p selClause)
                             (checkTableRef p tab))
             (combinePMs (,) (checkCondition p cond)
                             (checkGroup p gr))
checkSelHead p (Set setOp head1 head2) = 
  if (checkHeadsTogether head1 head2)
    then (combinePMs (\chHead1 chHead2 -> (Set setOp chHead1 chHead2))
                    (checkSelHead p head1)
                    (checkSelHead p head2))
    else throwPM p ("Queries combined by set operator have different "++
                    "result column types.")

checkHeadsTogether :: SelectHead -> SelectHead -> Bool
checkHeadsTogether (Query selclause1 _ _ _) (Query selclause2 _ _ _) =
    compareLists (extractElemTypes selclause1)
                 (extractElemTypes selclause2)
checkHeadsTogether head1 (Set _ head2 head3) =
      (checkHeadsTogether head1 head2) &&
        (checkHeadsTogether head2 head3)

extractElemTypes :: SelectClause -> [Type] 
extractElemTypes (SelAll _) = []
extractElemTypes (SelColumns _ elems) = (map getElemType elems)

getElemType :: SelElement -> Type
getElemType (Col col) = columnTyp col 
getElemType (Case _ (Left col) _ ) = columnTyp col 
getElemType (Case _ (Right val) _ ) = typeOf val
getElemType (Aggregation ASum _ col) = columnTyp col 
getElemType (Aggregation AAvg _ _) = F 
getElemType (Aggregation ACount _ _) = I
getElemType (Aggregation AMin _ col) = columnTyp col 
getElemType (Aggregation AMax _ col) = columnTyp col


checkSelClause :: Pos -> SelectClause -> PM SelectClause
checkSelClause _ sc@(SelAll _) = cleanPM sc
checkSelClause p (SelColumns sp elems) = 
    liftPM (\chElems -> (SelColumns sp chElems))
           (sequencePM (map (checkSelElement p) elems))

checkSelElement :: Pos -> SelElement -> PM SelElement
checkSelElement _ e@(Col _) = cleanPM e
checkSelElement p (Case cond op1 op2) = 
       combinePMs (\chCond (chOp1, chOp2) -> (Case chCond chOp1 chOp2))
                  (checkCondition p cond)
                  (checkOpOp p op1 op2)
checkSelElement p (Aggregation fun sp col) =
       liftPM (\chCol -> (Aggregation fun sp chCol))
              (checkFunCol p fun col) 

-- Checks the use of Aggregation functions in Selectelements
-- Avg and Sum can only be used with Int and Float
checkFunCol :: Pos -> AFun -> ColumnRef -> PM ColumnRef
checkFunCol p ASum col =
  case columnTyp col of
         I -> cleanPM col 
         F -> cleanPM col 
         _ -> throwPM p ("Inside function sum only columns of type Int"
                          ++" or float are allowed.")
checkFunCol p AAvg col =
  case columnTyp col of
         I -> cleanPM col 
         F -> cleanPM col 
         _ -> throwPM p ("Inside function avg only columns of type Int"
                          ++" or float are allowed.")
checkFunCol _ ACount col = cleanPM col 
checkFunCol _ AMin col = cleanPM col 
checkFunCol _ AMax col = cleanPM col

checkTableRef :: Pos -> TableRef -> PM TableRef
checkTableRef _ t@(TableRef _ Nothing) = cleanPM t
checkTableRef p (TableRef t (Just join)) = 
    liftPM (\chJoin -> (TableRef t (Just chJoin)))
           (checkJoinClause p join)

checkJoinClause :: Pos -> JoinClause -> PM JoinClause
checkJoinClause _ t@(CrossJoin _ Nothing) = cleanPM t
checkJoinClause p (CrossJoin tab (Just join)) =
    liftPM (\chJoin -> (CrossJoin tab (Just chJoin)))
           (checkJoinClause p join)
checkJoinClause p (InnerJoin t jCond Nothing) =
    liftPM (\chCond -> (InnerJoin t chCond Nothing))
           (checkJoinCond p jCond)
checkJoinClause p (InnerJoin t jCond (Just join)) =
    combinePMs (\chCond chJoin -> (InnerJoin t chCond (Just chJoin)))
               (checkJoinCond p jCond)
               (checkJoinClause p join)

checkJoinCond :: Pos -> JoinCond -> PM JoinCond
checkJoinCond p (JC cond) =
      liftPM (\chCond -> (JC chCond))
             (checkCondition p cond)


checkGroup :: Pos -> (Maybe Group) -> PM (Maybe Group)
checkGroup _ Nothing = cleanPM Nothing
checkGroup p (Just (GroupBy cols hav)) =
     liftPM (\chHave -> (Just (GroupBy cols chHave)))
            (checkHaving p hav)

checkHaving :: Pos -> Having -> PM Having
checkHaving _ NoHave = cleanPM NoHave
checkHaving p (CmpHave logOp hav1 hav2) =
    combinePMs (\chHav1 chHav2 -> (CmpHave logOp chHav1 chHav2))
               (checkHaving p hav1)
               (checkHaving p hav2)
checkHaving p (Neg have) = liftPM (\chHav -> (Neg chHav))
                                  (checkHaving p have)
checkHaving p (AggrHave fun sp col op operand) =
        combinePMs (\chCol chOp -> (AggrHave fun sp chCol op chOp))
                   (checkFunCol p fun col)
                   (checkAggrHave p fun col operand)
checkHaving p (SimpleHave cond) =
        liftPM (\chCond -> (SimpleHave chCond))
               (checkCondition p cond)

-- Checks the use of aggregation functions in having-clauses.
-- In case of Avg the operand has to be of type float, in case of 
-- Count of type int. In the remaining cases both operands have to be
-- of the same type.
checkAggrHave :: Pos -> AFun -> ColumnRef -> Operand -> PM Operand
checkAggrHave p ASum col op =
      liftPM (\(chOp, _) -> chOp)
             (checkOpCol p op col)
checkAggrHave p AAvg _ op =
      liftPM (\(chOp, _) -> chOp)
             (checkOpVal p op (FloatExp 0.0))
checkAggrHave p ACount _ op =
      liftPM (\(chOp, _) -> chOp)
             (checkOpVal p op (IntExp 0)) 
checkAggrHave p AMin col op =
      liftPM (\(chOp, _) -> chOp)
             (checkOpCol p op col)
checkAggrHave p AMax col op =
      liftPM (\(chOp, _) -> chOp)
             (checkOpCol p op col)

-- -------------------------- update - second stage ---------------------------
-- Tries to find table name as part of the embedded expression 
-- (e.g. as Constructor).
-- If unsuccesful type is set using the table name and a warning is thrown.
checkEntUpdate :: Pos -> Table -> Value -> PM Value
checkEntUpdate p (Table name _ _ ) (Emb exp _) =
   if (isInfixOf (firstUp name) exp)
      then cleanPM (Emb exp (Entity (firstUp name))) 
      else warnOKPM (Emb exp (Entity (firstUp name))) 
                    [(p, ("Embedded expression in update statement was set"
                       ++ " to type "++(firstUp name)))]

checkAssignments :: Pos -> [Assign] -> PM [Assign]
checkAssignments p assigns = sequencePM (map (checkAssign p) assigns)

-- Ensures Key- and null-values are not set directly in an update statement.
checkAssign :: Pos -> Assign -> PM Assign
checkAssign p (Assign col@(Column (Unique tab) _ t _ _) val) = 
  case val of
     AbsNull -> throwPM p ("Setting null-values directly is not supported by"
                               ++" the CDBI-Interface, use update of whole "++
                                 "entity instead.")
     _       -> case t of 
                  (Key tn) -> if tn == tab
                                then throwPM p ("Setting key-values directly "
                                                   ++"is not supported by the "
                                                   ++"CDBI-Interface.")
                                else liftPM (\(chCol, chVal) -> 
                                                   Assign chCol chVal)
                                              (checkColVal p col val)                
                  _       -> liftPM (\(chCol, chVal) -> Assign chCol chVal)
                                    (checkColVal p col val)
-- this should not happen at this stage anymore.
checkAssign p (Assign col@(Column (Def _ ) name _ _ _) _) = 
    throwPM p ("Column "++name++" could not be related to any table.")
                                       

-- -------------------------- insert - second stage ---------------------------

-- In case an embedded expression is given: tries to find table name as part
-- of the embedded expression (e.g. as Constructor).
-- If unsuccesful type is set using the table name and a warning is thrown.
-- Otherwise types of columns are compared with types of values.
checkInsertVals :: Pos -> Table -> [ColumnRef] -> [[Value]] -> PM [[Value]]
checkInsertVals p tab cols valss = 
      sequencePM (map (compareInserts p tab cols) valss)

compareInserts :: Pos -> Table -> [ColumnRef] -> [Value] -> PM [Value]
compareInserts p (Table name _ _) cols vals =
  case vals of
      [(Emb exp _ )] ->  if (isInfixOf (firstUp name) exp) -- is type constructor part of exp?
                           then cleanPM [(Emb exp (Entity(firstUp name)))] 
                           else warnOKPM [(Emb exp (Entity(firstUp name)))]  
                                         [(p,("Type of embedded expression "
                                              ++"was set to "++(firstUp name)
                                              ++" during preprocessing."))]                       
      _               -> compareInserts' p cols vals 

compareInserts' :: Pos -> [ColumnRef] -> [Value] -> PM [Value]
compareInserts' _ [] _ = cleanPM []
compareInserts' p (c:cols) (v:vals) = 
  case v of
   AbsNull      -> liftPM (\chVals -> (v:chVals))
                          (compareInserts' p cols vals)
   (KeyExp _ _) -> liftPM (\chVals -> (v:chVals))
                          (compareInserts' p cols vals)                       
   (IntExp int) -> case columnTyp c of 
                     (Key tab) -> liftPM (\chVals -> ((KeyExp tab int):chVals))
                                         (compareInserts' p cols vals)
                     _        -> combinePMs (\(_, chVal) chVals -> (chVal:chVals))
                                             (checkColVal p c v)
                                             (compareInserts' p cols vals) 
   _            -> combinePMs (\(_, chVal) chVals -> (chVal:chVals))
                              (checkColVal p c v)
                              (compareInserts' p cols vals) 


-- --------------------------- common elements - second stage ----------------------

checkCondition :: Pos -> Condition -> PM Condition
checkCondition _ NoCond = cleanPM NoCond
checkCondition _ c@(FK _ _ _) = cleanPM c
checkCondition p (Cmp logOp cond1 cond2) = 
       combinePMs (\chCond1 chCond2 -> (Cmp logOp chCond1 chCond2))
                  (checkCondition p cond1)
                  (checkCondition p cond2)
checkCondition p (Not cond) = liftPM (\chCond -> (Not chCond))
                                     (checkCondition p cond)
checkCondition p (Exists stat) = liftPM (\chStat -> (Exists chStat))
                                        (checkSubquery p stat)
checkCondition _ c@(IsNull _) = cleanPM c
checkCondition _ c@(NotNull _) = cleanPM c
checkCondition p (AIn op vals) = 
        liftPM (\chOpVals -> (AIn (fst(head chOpVals)) (map snd chOpVals)))
               (sequencePM (map (checkOpVal p op) vals))
checkCondition p (ABinOp binOp op1 op2) =
        liftPM (\(chOp1, chOp2) -> (ABinOp binOp chOp1 chOp2))
               (checkOpOp p op1 op2)
checkCondition p (ABetween op1 op2 op3) =
        combinePMs (\(chOp1, chOp2) (_, chOp3) -> (ABetween chOp1 chOp2 chOp3))
                   (checkOpOp p op1 op2)
                   (checkOpOp p op1 op3)

checkSubquery :: Pos -> Statement -> PM Statement
checkSubquery p (Select selHead order limit) =
  liftPM (\chSelHead -> (Select chSelHead order limit))
         (checkSelHead p selHead)

-- Compares type of operand with type of value
checkOpVal :: Pos -> Operand -> Value -> PM (Operand, Value)
checkOpVal p (Right val1) val2 = 
         liftPM (\(chVal1, chVal2) -> ((Right chVal1), chVal2))
                (checkValVal p val1 val2)
checkOpVal p (Left col) val = 
         liftPM (\(chCol, chVal) -> ((Left chCol),chVal))
                (checkColVal p col val)

-- Compares type of Operand to type of Column
checkOpCol :: Pos -> Operand -> ColumnRef -> PM (Operand, ColumnRef)
checkOpCol p (Right val) col = 
         liftPM (\(chCol, chVal) -> ((Right chVal), chCol))
                (checkColVal p col val) 
checkOpCol p (Left col1) col2 = 
         liftPM (\(chCol1, chCol2) -> ((Left chCol1), chCol2))
                (checkColCol p col1 col2)

-- Compares types of two Operands
checkOpOp :: Pos -> Operand -> Operand -> PM (Operand, Operand)
checkOpOp p (Right val1) (Right val2) = 
     liftPM (\(chVal1, chVal2) -> ((Right chVal1), (Right chVal2))) 
            (checkValVal p val1 val2)
checkOpOp p (Right val) (Left col) = 
     liftPM (\(chCol, chVal) -> ((Right chVal), (Left chCol))) 
            (checkColVal p col val) 
checkOpOp p (Left col) (Right val) = 
     liftPM (\(chCol, chVal) -> ((Left chCol), (Right chVal))) 
            (checkColVal p col val)
checkOpOp p (Left col1) (Left col2) = 
     liftPM (\(chCol1, chCol2) -> ((Left chCol1), (Left chCol2))) 
            (checkColCol p col1 col2)

-- Compares types of two values.
-- Throws an error if they are not of the same type, if a null-value or
-- key value is found. Throws a warning if type of embedded expression is
-- set using context information.
checkValVal :: Pos -> Value -> Value -> PM (Value, Value)
checkValVal p v1@(IntExp int) v2 = 
   case v2 of
     (IntExp _)  -> cleanPM (v1,v2)
     (Emb exp _) -> warnOKPM (v1, (Emb exp I)) 
                             [(p, ("Information: embedded expression is "++
                                   "assumed to be of type Int"))]
     _           -> throwPM p ("Type error: Int ("++(show int)++") and "++
                               (typeToStr (typeOf v2))++" are not compatible.")
checkValVal p v1@(FloatExp f) v2 = 
   case v2 of
     (FloatExp _) -> cleanPM (v1,v2)
     (Emb exp _)  -> warnOKPM (v1, (Emb exp F)) 
                              [(p, ("Information: embedded expression is "++
                                    "assumed to be of type Float"))]
     _            -> throwPM p ("Type error: Float ("++(show f)++") and "++
                                (typeToStr (typeOf v2))++" are not compatible.")
checkValVal p v1@(StringExp str) v2 = 
   case v2 of
     (StringExp _) -> cleanPM (v1,v2)
     (Emb exp _)   -> warnOKPM (v1, (Emb exp S)) 
                               [(p, ("Information: embedded expression is "++
                                      "assumed to be of type String"))]
     _             -> throwPM p ("Type error: String ("++str++") and "++
                                 (typeToStr (typeOf v2))++" are not compatible.")
checkValVal p v1@(DateExp d) v2 = 
   case v2 of
     (DateExp _)  -> combinePMs (\date1 date2 -> (date1,date2))
                                (checkDate p v1)
                                (checkDate p v2)
     (Emb exp _)  -> warnOKPM (v1, (Emb exp D)) 
                              [(p, ("Information: embedded expression is "++
                                    "assumed to be of type Date"))]
     _            -> throwPM p ("Type error: Date ("++(show d)++") and "++
                                (typeToStr (typeOf v2))++" are not compatible.")
checkValVal p v1@(BoolExp b) v2 = 
   case v2 of
     (BoolExp _)  -> cleanPM (v1,v2)
     (Emb exp _)  -> warnOKPM (v1, (Emb exp B)) 
                              [(p, ("Information: embedded expression is "++
                                    "assumed to be of type Bool"))]
     _            -> throwPM p ("Type error: Boolean ("++(show b)++") and "++
                                (typeToStr (typeOf v2))++" are not compatible.")
checkValVal p v1@(CharExp c) v2 =
   case v2 of
     (CharExp _)  -> cleanPM (v1,v2)
     (Emb exp _)  -> warnOKPM (v1, (Emb exp C)) 
                              [(p, ("Information: embedded expression is "++
                                    "assumed to be of type Char"))]
     _            -> throwPM p ("Type error: Char ("++(show c)++") and "++
                                (typeToStr (typeOf v2))++" are not compatible.")
checkValVal p v1@(Emb _ _) v2 =
   case v2 of
      (Emb _ _) -> throwPM p ("Because of type safety it is "
                               ++" not possible to compare "
                               ++"two embedded expressions.")
      _         -> checkValVal p v2 v1
checkValVal p (KeyExp _ _) _ = throwPM p ("The CDBI-Interface does not allow"
                                           ++" the use of (foreign) key values "
                                           ++"in case exp.")-- this is actually not possible
                                          --cause KeyExp are dependent on a referencing column      
checkValVal p (AbsNull) _ = throwPM p ("The use of Null-values in "
                                        ++"case-expressions is not "
                                        ++"supported by the CDBI-Interface.")

-- Compares type of a column to type of a value.
-- Throws an error if they are not of the same type. 
-- Throws a warning if type of embedded expression is
-- set using context information.
checkColVal :: Pos -> ColumnRef -> Value -> PM (ColumnRef, Value)
checkColVal p c@(Column _ col I _ _) val = 
   case val of
      (IntExp _)  -> cleanPM (c,val)
      (Emb exp _) -> warnOKPM (c, (Emb exp I)) 
                              [(p, ("Information: embedded expression is "++
                                    "assumed to be of type Int"))]
      _           -> throwPM p ("Type error: Int ("++col++") and "++
                                (typeToStr (typeOf val))++" are not compatible.")
checkColVal p c@(Column _ col F _ _) val = 
   case val of
      (FloatExp _)-> cleanPM (c,val)
      (Emb exp _) -> warnOKPM (c, (Emb exp F)) 
                              [(p, ("Information: embedded expression is "++
                                    "assumed to be of type Float"))]
      _           -> throwPM p ("Type error: Float ("++col++") and "++
                                (typeToStr (typeOf val))++" are not compatible.")
checkColVal p c@(Column _ col B _ _) val = 
   case val of
      (BoolExp _) -> cleanPM (c,val)
      (Emb exp _) -> warnOKPM (c, (Emb exp B)) 
                              [(p, ("Information: embedded expression is "++
                                    "assumed to be of type Bool"))]
      _           -> throwPM p ("Type error: Bool ("++col++") and "++
                                (typeToStr (typeOf val))++" are not compatible.")
checkColVal p c@(Column _ col C _ _) val = 
   case val of
      (CharExp _) -> cleanPM (c,val)
      (Emb exp _) -> warnOKPM (c, (Emb exp C)) 
                              [(p, ("Information: embedded expression is "++
                                    "assumed to be of type Char"))]
      _           -> throwPM p ("Type error: Char ("++col++") and "++
                                (typeToStr (typeOf val))++" are not compatible.")
checkColVal p c@(Column _ col S _ _) val = 
   case val of
      (StringExp _) -> cleanPM (c,val)
      (Emb exp _) -> warnOKPM (c, (Emb exp S)) 
                              [(p, ("Information: embedded expression is "++
                                    "assumed to be of type String"))]
      _           -> throwPM p ("Type error: String ("++col++") and "++
                                (typeToStr (typeOf val))++" are not compatible.")
checkColVal p c@(Column _ col D _ _) val = 
   case val of
      (DateExp _) -> liftPM (\date -> (c, date))
                            (checkDate p val)
      (Emb exp _) -> warnOKPM (c, (Emb exp D)) 
                              [(p, ("Information: embedded expression is "++
                                    "assumed to be of type Date"))]
      _           -> throwPM p ("Type error: Date ("++col++") and "++
                                (typeToStr (typeOf val))++" are not compatible.")
checkColVal p c@(Column _ _ (Key reftab) _ _) val = 
    case val of
       (IntExp i) -> cleanPM (c, (KeyExp reftab i))
       (Emb exp _) -> warnOKPM (c, (Emb exp (Key reftab))) 
                               [(p, ("Information: embedded expression is "++
                                     "assumed to be of type "
                                      ++(firstUp reftab)++"ID"))]
       _           -> throwPM p ("Type error: "++reftab++"ID and "++
                                (typeToStr (typeOf val))++" are not compatible.")

-- Compares the type of two columns. Throws an error if they are not the same.
checkColCol :: Pos -> ColumnRef -> ColumnRef -> PM (ColumnRef, ColumnRef)
checkColCol p c1@(Column _ col I _ _) c2 =
   case columnTyp c2 of
             I  -> cleanPM (c1, c2)
             _  -> throwPM p ("Type error: column of type Int ("++col
                               ++") was combined with column of type "++
                                 (typeToStr (columnTyp c2)))
checkColCol p c1@(Column _ col F _ _) c2 =
   case columnTyp c2 of
             F  -> cleanPM (c1, c2)
             _  -> throwPM p ("Type error: column of type Float ("++col++
                               ") was combined with column of type "++
                                 (typeToStr (columnTyp c2)))
checkColCol p c1@(Column _ col B _ _) c2 =
   case columnTyp c2 of
             B  -> cleanPM (c1, c2)
             _  -> throwPM p ("Type error: column of type Bool ("++col++
                               ") was combined with column of type "++
                                 (typeToStr (columnTyp c2)))
checkColCol p c1@(Column _ col C _ _) c2 =
   case columnTyp c2 of
             C  -> cleanPM (c1, c2)
             _  -> throwPM p ("Type error: column of type Char ("++col++
                               ") was combined with column of type "++
                                 (typeToStr (columnTyp c2)))
checkColCol p c1@(Column _ col S _ _) c2 =
   case columnTyp c2 of
             S  -> cleanPM (c1, c2)
             _  -> throwPM p ("Type error: column of type String ("++col++
                               ") was combined with column of type "++
                                 (typeToStr (columnTyp c2)))
checkColCol p c1@(Column _ col D _ _) c2 =
   case columnTyp c2 of
             D  -> cleanPM (c1, c2)
             _  -> throwPM p ("Type error: column of type Date ("++col++
                               ") was combined with column of type "++
                                 (typeToStr (columnTyp c2)))
checkColCol p c1@(Column _ _ (Key tab1) _ _) c2 =
    case columnTyp c2 of
           (Key tab2) -> if tab1 == tab2
                           then cleanPM (c1,c2)
                           else throwPM p ("Type error: foreign keys are "
                                           ++"referencing different tables: "
                                           ++ tab1++" and "++tab2)
           _          -> throwPM p ("Type error: column of type "++
                                    (typeToStr (columnTyp c1))++ 
                                    " was combined with column of type "++
                                    (typeToStr (columnTyp c2)))

checkDate :: Pos -> Value -> PM Value
checkDate p date = 
  case date of
    (DateExp (CalendarTime y m d _ _ _ _ )) ->
              if (y >= 1970) && (1 <= m) && (m <= 12) && (1<= d) && (d <= 31)
                then cleanPM date
                else warnOKPM (date) 
                     [(p, ("It seems like your date-expression is not in the"++
                           " correct form: year(four-digit) month day hour"++
                           " minute second"))]
    _  -> cleanPM date

typeToStr :: Type -> String
typeToStr I = "Int"
typeToStr F = "Float"
typeToStr B = "Boolean"
typeToStr C = "Char"
typeToStr D = "Date"
typeToStr S = "String"
typeToStr Unknown = "Unknown (Null or embedded)"
typeToStr (Key tab) = (firstUp tab)++"ID"
typeToStr (Entity name) = name

columnTyp :: ColumnRef -> Type
columnTyp (Column _ _ t _ _) = t

typeOf :: Value -> Type
typeOf (IntExp _) = I
typeOf (FloatExp _) = F
typeOf (StringExp _) = S
typeOf (DateExp _) = D
typeOf (BoolExp _) = B
typeOf (CharExp _ ) = C
typeOf (Emb _ _) = Unknown
typeOf AbsNull = Unknown
typeOf (KeyExp tab _) = (Key tab)

--auxiliary functions --------------------------

firstUp :: String -> String
firstUp [] = ""
firstUp (s:str) = (toUpper s):str

firstLow :: String -> String
firstLow [] = ""
firstLow (s:str) = (toLower s):str

compareLists :: Eq a => [a] -> [a] -> Bool
compareLists [] [] = True
compareLists [] (_:_) = False
compareLists (_:_) [] = False
compareLists (a:as) (b:bs) = if (a == b)
                               then compareLists as bs
                               else False 
   