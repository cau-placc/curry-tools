--- This module defines the Token needed to scan an embedded
--- SQL Statement. In Addition it defines a 'ToString'-method 
--- for all token.
--- The supported datatypes, functions, operator etc. are based on
--- the CDBI-Interface.
---@author Julia Krone
---@version 0.1
-- -----------------------------------------------------------

module SQLToken where

import Time (CalendarTime)


data Token
  --identifier
    = Constant ConstVal
    | Ident String
    | EmbedExp String 
  --Keywords
    | KW_Select
    | KW_From
    | KW_Where
    | KW_Order
    | KW_Group
    | KW_By
    | KW_Having
    | KW_Insert
    | KW_Into
    | KW_Values
    | KW_Update
    | KW_Set
    | KW_Delete
    | KW_Transaction
    | KW_Inner
    | KW_Cross
    | KW_Join
    | KW_On
    | KW_Satisfies
    | KW_As
    | KW_Distinct
    | KW_All
    | KW_Case
    | KW_When
    | KW_Then
    | KW_Else
    | KW_End
    | KW_Asc
    | KW_Desc
    | KW_Limit
    | KW_Exists
    | KW_Not  
    | KW_Commit
    | KW_Rollback
    | KW_Begin
    | KW_Table
  --Operators
    | Fun Func
    | BinOp BinOperator
    | LogOp LogOperator
    | SetOp SetOperator
    | Is
    | Between
    | In
  --Punctuation
    | LParen
    | RParen
    | Comma
    | Semi
    | Stop
    | Asterix
   -- unsupported Char
    | Unexpected Char
 deriving Eq

data ConstVal = NumInt Int 
              | NumFloat Float 
              | Boolean Bool 
              | Date CalendarTime 
              | VarStr String 
              | VarChar Char
              | Null
 deriving Eq

data Func = Sum | Avg | Min | Max | Count
 deriving Eq
 
data BinOperator = Lth | Gth | Lte | Gte | Equal | Uneq | Like
 deriving Eq

data LogOperator = And | Or 
 deriving Eq

data SetOperator = Union | Intersect | Except
 deriving Eq

--auxiliary function to print token for error messages
tokentoString :: Token -> String
tokentoString t = case t of
                       Constant (NumInt int)     -> "interger " ++ show int 
                       Constant (NumFloat float) -> "float " ++ show float
                       Constant (Date date)      -> "date " ++ show date
                       Constant (Boolean bool)   -> "boolean " ++ show bool
                       Constant (VarChar char)   -> "char "++ show char
                       Ident str                 -> "identifier " ++ str
                       Constant (VarStr str)     -> "string " ++ str
                       EmbedExp exp              -> ("embedded curry  "++
                                                       "expression" ++ exp)
                       Constant Null             -> "null"
                       KW_Select                 -> "Select"
                       KW_From                   -> "From"
                       KW_Where                  -> "Where"
                       KW_Order                  -> "Order"
                       KW_Group                  -> "Group"
                       KW_By                     -> "By"
                       KW_Having                 -> "Having"
                       KW_Insert                 -> "Insert"
                       KW_Into                   -> "Into"
                       KW_Values                 -> "Values"
                       KW_Update                 -> "Update"
                       KW_Set                    -> "Set"
                       KW_Delete                 -> "Delete"
                       KW_Transaction            -> "Transaction"
                       KW_Begin                  -> "Begin"
                       KW_Inner                  -> "Inner"
                       KW_Cross                  -> "Cross"
                       KW_Join                   -> "Join"
                       KW_On                     -> "On"
                       KW_Satisfies              -> "Satisfies"
                       KW_As                     -> "As"
                       KW_Distinct               -> "Distinct"
                       KW_All                    -> "All"                     
                       KW_Case                   -> "Case"
                       KW_When                   -> "When"
                       KW_Then                   -> "Then"
                       KW_Else                   -> "Else" 
                       KW_End                    -> "End"
                       KW_Commit                 -> "Commit"
                       KW_Rollback               -> "Rollback"
                       KW_Table                  -> "keyword table"
                       Fun Count                 -> "Count"
                       Fun Avg                   -> "Avg"
                       Fun Min                   -> "Min"
                       Fun Max                   -> "Max"
                       Fun Sum                   -> "Sum"
                       KW_Asc                    -> "Asc"
                       KW_Desc                   -> "Desc"
                       KW_Limit                  -> "Limit"
                       KW_Exists                 -> "Exists"
                       KW_Not                    -> "Not"   
                       BinOp Lth                 -> "<"
                       BinOp Lte                 -> "<="
                       BinOp Gth                 -> ">"
                       BinOp Gte                 -> ">="
                       BinOp Equal               -> "="
                       BinOp Uneq                -> "!="
                       BinOp Like                -> "like"
                       LogOp And                 -> "And"
                       LogOp Or                  -> "Or"
                       SetOp Union               -> "Union"
                       SetOp Intersect           -> "Intersect"
                       SetOp Except              -> "Except"
                       Between                   -> "between"
                       In                        -> "In"
                       Is                        -> "Is"
                       LParen                    -> "("
                       RParen                    -> ")"
                       Semi                      -> ";"
                       Comma                     -> ","
                       Stop                      -> "."
                       Asterix                   -> "*"
                       Unexpected c              -> ("Unsupported Character: "
                                                                      ++(c:""))
                     