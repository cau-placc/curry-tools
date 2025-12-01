------------------------------------------------------------------------------
-- | Author : Michael Hanus
--   Version: November 2025
--
-- This library supports meta-programming, i.e., the manipulation of
-- Curry programs in Curry. For this purpose, the library contains
-- definitions of data types for the representation of Curry programs
-- in the FlatCurry format where all function definitions are
-- at the top-level and pattern matching is replaced by case expressions
-- and disjunctions.
------------------------------------------------------------------------------

module FlatCurry.Types where

-- | Data type for representing a Curry module in the intermediate form.
--   A value of this data type has the form
--  
--       (Prog modname imports typedecls functions opdecls)
--  
--   where
--   `modname` is the name of this module,
--   `imports` is the list of modules names that are imported, and
--   `typedecls`, `functions`, and `opdecls` are the list of
--   data type, function, and operator declarations
--   contained in this module, respectively.
data Prog = Prog String [String] [TypeDecl] [FuncDecl] [OpDecl]
  deriving (Eq, Ord, Read, Show)

-- | The data type for representing qualified names.
--   In FlatCurry all names are qualified to avoid name clashes.
--   The first component is the module name and the second component the
--   unqualified name as it occurs in the source program.
type QName = (String, String)

-- | Data type to specify the visibility of various entities.
data Visibility
  = Public    -- public (exported) entity
  | Private   -- private entity
 deriving (Eq, Ord, Read, Show)

-- | The data type for representing type variables.
--   They are represented by `(TVar i)` where `i` is a type variable index.
type TVarIndex = Int

-- | Kinded type variables are represented by a tuple of type variable
--   index and kind.
type TVarWithKind = (TVarIndex, Kind)

-- | Data type for representing definitions of algebraic data types
--   and type synonyms.
--  
--   A data type definition of the form
--  
--       data t x1...xn = ...| c t1....tkc |...
--  
--   is represented by the FlatCurry term
--  
--       (Type t [i1,...,in] [...(Cons c kc [t1,...,tkc])...])
--  
--   where each `ij` is the index of the type variable `xj`.
--  
--   Note: the type variable indices are unique inside each type declaration
--         and are usually numbered from 0
--  
--   Thus, a data type declaration consists of the name of the data type,
--   a list of type parameters and a list of constructor declarations.
data TypeDecl
  = Type    QName Visibility [TVarWithKind] [ConsDecl]
  | TypeSyn QName Visibility [TVarWithKind] TypeExpr
  | TypeNew QName Visibility [TVarWithKind] NewConsDecl
  deriving (Eq, Ord, Read, Show)

-- | A constructor declaration consists of the name and arity of the
--   constructor and a list of the argument types of the constructor.
data ConsDecl = Cons QName Int Visibility [TypeExpr]
  deriving (Eq, Ord, Read, Show)

-- | A constructor declaration for a newtype consists
--   of the name of the constructor
--   and the argument type of the constructor.
data NewConsDecl = NewCons QName Visibility TypeExpr
    deriving (Eq, Ord, Read, Show)

-- | Data type for type expressions.
--   A type expression is either a type variable, a function type,
--   or a type constructor application.
--  
--   Note: the names of the predefined type constructors are
--   "Int", "Float", "Bool", "Char", "IO",
--   "()" (unit type), "(,...,)" (tuple types), "[]" (list type)
data TypeExpr
  = TVar TVarIndex                      -- ^ type variable
  | FuncType TypeExpr TypeExpr          -- ^ function type t1->t2
  | TCons QName [TypeExpr]              -- ^ type constructor application
                                        --   `TCons (module,name) typeargs`
  | ForallType  [TVarWithKind] TypeExpr -- ^ forall type
 deriving (Eq, Ord, Read, Show)

-- | The kind of a type variable.
data Kind
  = KStar             -- ^ kind of concrete types (e.g., `Bool`, `Int`)
  | KArrow Kind Kind  -- ^ kind of a type constructor (e.g., `Maybe`)
 deriving (Eq, Ord, Read, Show)

-- | Data type for operator declarations.
--   An operator declaration `fix p n` in Curry corresponds to the
--   FlatCurry term `(Op n fix p)`.
data OpDecl = Op QName Fixity Int
 deriving (Eq, Ord, Read, Show)

-- | Data types for the different choices for the fixity of an operator.
data Fixity = InfixOp | InfixlOp | InfixrOp
 deriving (Eq, Ord, Read, Show)

-- | Data type for representing object variables.
--   Object variables occurring in expressions are represented by `(Var i)`
--   where `i` is a variable index.
type VarIndex = Int

-- | Arity of a function.
type Arity = Int

-- | Data type for representing function declarations.
--  
--   A function declaration in FlatCurry is a term of the form
--  
--       (Func name k type (Rule [i1,...,ik] e))
--  
--   and represents the function `name` with definition
--  
--       name :: type
--       name x1...xk = e
--  
--   where each `ij` is the index of the variable `xj`.
--  
--   Note: the variable indices are unique inside each function declaration
--         and are usually numbered from 0
--  
--   External functions are represented as
--  
--       (Func name arity type (External s))
--  
--   where s is the external name associated to this function.
--  
--   Thus, a function declaration consists of the name, arity, type, and rule.
data FuncDecl = Func QName Arity Visibility TypeExpr Rule
 deriving (Eq, Ord, Read, Show)

-- | A rule is either a list of formal parameters together with an expression
--   or externally defined.
data Rule
  = Rule [VarIndex] Expr -- ^ a rule with formal parameters and body expression
  | External String      -- ^ an externally defined function with an external
                         --   name
 deriving (Eq, Ord, Read, Show)

-- | Data type for classifying case expressions.
--   Case expressions can be either flexible or rigid in Curry.
data CaseType = Rigid -- ^ rigid case expression
              | Flex  -- ^ flexible case expression
 deriving (Eq, Ord, Read, Show)

-- | Data type for classifying combinations
--   (i.e., a function/constructor applied to some arguments).
data CombType =
    FuncCall
  -- ^ a call to a function where all arguments are provided
  | ConsCall
  -- ^ a call with a constructor at the top, all arguments are provided
  | FuncPartCall Arity
  -- ^ a partial call to a function (i.e., not all arguments are provided)
  --   where the parameter is the number of missing arguments
  | ConsPartCall Arity
  -- ^ a partial call to a constructor (i.e., not all arguments are provided)
  --   where the parameter is the number of missing arguments
 deriving (Eq, Ord, Read, Show)

-- | Data type for representing expressions.
--  
--   Remarks:
--  
--   if-then-else expressions are represented as rigid case expressions:
--  
--       (if e1 then e2 else e3)
--  
--   is represented as
--  
--       (case e1 of { True -> e2; False -> e3})
--  
--   Higher-order applications are represented as calls to the (external)
--   function `apply`. For instance, the rule
--  
--       app f x = f x
--  
--   is represented as
--  
--       (Rule  [0,1] (Comb FuncCall ("Prelude","apply") [Var 0, Var 1]))
--  
--   A conditional rule is represented as a call to an external function
--   `cond` where the first argument is the condition (a constraint).
--   For instance, the rule
--  
--       equal2 x | x=:=2 = True
--  
--   is represented as
--  
--       (Rule [0]
--             (Comb FuncCall ("Prelude","cond")
--                   [Comb FuncCall ("Prelude","=:=") [Var 0, Lit (Intc 2)],
--                    Comb FuncCall ("Prelude","True") []]))
data Expr
  = Var VarIndex
  -- ^ variable (represented by unique index)
  | Lit Literal
  -- ^ literal (Int/Float/Char constant)
  | Comb CombType QName [Expr]
  -- ^ application `(f e1 ... en)` of a function or constructor `f`
  --  with `n`&lt;=arity(`f`)
  | Let [(VarIndex, TypeExpr,Expr)] Expr
  -- ^ introduction of local variables via (recursive) let declarations
  | Free [(VarIndex,TypeExpr)] Expr
  -- ^ introduction of free local variables
  | Or Expr Expr
  -- ^ disjunction of two expressions (used to translate rules with
  --   overlapping left-hand sides)
  | Case CaseType Expr [BranchExpr]
  -- ^ case distinction (rigid or flex)
  | Typed Expr TypeExpr
  -- ^ typed expression to represent an expression with a type annotation
 deriving (Eq, Ord, Read, Show)

-- | Data type for representing branches in a case expression.
--  
--   Branches "(m.c x1...xn) -> e" in case expressions are represented as
--  
--       (Branch (Pattern (m,c) [i1,...,in]) e)
--  
--   where each `ij` is the index of the pattern variable `xj`, or as
--  
--       (Branch (LPattern (Intc i)) e)
--  
--   for integers as branch patterns (similarly for other literals
--   like float or character constants).
data BranchExpr = Branch Pattern Expr
 deriving (Eq, Ord, Read, Show)

-- | Data type for representing patterns in case expressions.
data Pattern
  = Pattern QName [VarIndex] -- ^ constructor pattern
  | LPattern Literal         -- ^ literal pattern
 deriving (Eq, Ord, Read, Show)

-- | Data type for representing literals occurring in an expression
--   or case branch. It is either an integer, a float, or a character constant.
data Literal
  = Intc   Int   -- ^ an integer literal
  | Floatc Float -- ^ a float literal
  | Charc  Char  -- ^ a character literal
 deriving (Eq, Ord, Read, Show)

------------------------------------------------------------------------------
-- | The bound variable of a single let binding.
varOfLetBind :: (VarIndex, TypeExpr,Expr) -> VarIndex
varOfLetBind (v,_,_) = v

-- | The list of all bound variables of a let binding.
varsOfLetBind :: [(VarIndex, TypeExpr,Expr)] -> [VarIndex]
varsOfLetBind = map varOfLetBind

-- | The bound expression of a single let binding.
expOfLetBind :: (VarIndex, TypeExpr,Expr) -> Expr
expOfLetBind (_,_,be) = be

-- | The list of all bound expressions of a let binding.
expsOfLetBind :: [(VarIndex, TypeExpr,Expr)] -> [Expr]
expsOfLetBind = map expOfLetBind

-- | Shows a qualified type name as a name relative to a module
--   (first argument). Thus, names not defined in this module (except for names
--   defined in the prelude) are prefixed with their module name.
showQNameInModule :: String -> QName -> String
showQNameInModule mname qn@(qmod, name)
  | qmod == mname || qmod == "Prelude" = name
  | otherwise                          = showQName qn

-- | Shows a qualified name.
showQName :: QName -> String
showQName (qmod, name) = qmod ++ '.' : name

-- | Transforms a name into Prelude-qualified name.
pre :: String -> QName
pre f = ("Prelude",f)

------------------------------------------------------------------------------
