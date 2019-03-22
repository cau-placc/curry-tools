------------------------------------------------------------------------------
--- RootReplaced analysis:
--- This analysis returns for each function f all functions to which this can
--- be replaced at the root. For instance, if there are the definitions:
---
---     f x = g x
---     g x = h x
---     h x = k x : []
---
--- then the root replacements of f are [g,h].
---
--- This analysis could be useful to detect simple loops, e.g., if
--- a function is in its root replacement. This is the purpose
--- of the analysis `RootCyclic` which assigns `True` to some
--- operation if this operation might cause a cyclic root replacement.
---
--- @author Michael Hanus
--- @version January 2017
------------------------------------------------------------------------------

module Analysis.RootReplaced
  ( rootReplAnalysis, showRootRepl
  , rootCyclicAnalysis, showRootCyclic
  )
 where

import Analysis.Types
import Analysis.ProgInfo
import FlatCurry.Types
import Data.List
------------------------------------------------------------------------------
--- Data type to represent root replacement information.
--- Basically, it is the set (represented as a sorted list) of
--- all function names to which a function can be replaced (directly
--- or by several steps) at the root
--- together with a list of arguments (which are numbered from 0)
--- which might be projected into the result.
--- The latter is necessary to compute the root replacement
--- information for definitions like `look = id loop`.
type RootReplaced = ([QName],[Int])

-- Show root-replacement information as a string.
showRootRepl :: AOutFormat -> RootReplaced -> String
showRootRepl AText ([],_)   = "no root replacements"
showRootRepl ANote ([],_)   = ""
showRootRepl AText (xs@(_:_),_) =
  "root replacements: " ++ intercalate "," (map (\ (mn,fn) -> mn++"."++fn) xs)
showRootRepl ANote (xs@(_:_),_) = "[" ++ intercalate "," (map snd xs) ++ "]"

--- Root replacement analysis.
rootReplAnalysis :: Analysis RootReplaced
rootReplAnalysis = dependencyFuncAnalysis "RootReplaced" ([],[]) rrFunc

rrFunc :: FuncDecl -> [(QName,RootReplaced)] -> RootReplaced
rrFunc (Func _ _ _ _ rule) calledFuncs = rrFuncRule calledFuncs rule

rrFuncRule :: [(QName,RootReplaced)] -> Rule -> RootReplaced
rrFuncRule _ (External _) = ([],[]) -- nothing known about external functions
rrFuncRule calledFuncs (Rule args rhs) = rrOfExp rhs
 where
  rrOfExp exp = case exp of
    Var v -> maybe ([],[]) (\i -> ([],[i])) (elemIndex v args)
    Lit _ -> ([],[])
    Comb ct g gargs ->
      if ct == FuncCall
       then maybe (error $ "Abstract value of " ++ show g ++ " not found!")
                  (\ (grrs,gps) ->
                    foldr lub (if g `elem` grrs
                                         then grrs
                                         else insertBy (<=) g grrs, [])
                              (map (\pi -> rrOfExp (gargs!!pi)) gps))
                  (lookup g calledFuncs)
       else ([],[])
    Typed e  _  -> rrOfExp e
    Free  _  e  -> rrOfExp e
    Let   _  e  -> rrOfExp e
    Or    e1 e2 -> lub (rrOfExp e1) (rrOfExp e2)
    Case _ e bs -> foldr lub (rrOfExp e)
                             (map (\ (Branch _ be) -> rrOfExp be) bs)

  lub (rr1,p1) (rr2,p2) = (sort (union rr1 rr2), sort (union p1 p2))

------------------------------------------------------------------------------
-- Show root-cyclic information as a string.
showRootCyclic :: AOutFormat -> Bool -> String
showRootCyclic AText False = "no cycles at the root"
showRootCyclic ANote False = ""
showRootCyclic AText True  = "possible cyclic root replacement"
showRootCyclic ANote True  = "root-cyclic"

--- Root cyclic analysis.
rootCyclicAnalysis :: Analysis Bool
rootCyclicAnalysis =
  combinedSimpleFuncAnalysis "RootCyclic" rootReplAnalysis rcFunc

rcFunc :: ProgInfo RootReplaced -> FuncDecl -> Bool
-- we assume that external functions are not root cyclic:
rcFunc _ (Func _  _ _ _ (External _)) = False
-- otherwise we check whether the operation is in its set of root replacements:
rcFunc rrinfo (Func qf _ _ _ (Rule _ _)) =
  maybe True -- no information, but this case should not occur
        (\rrfuncs -> qf `elem` (fst rrfuncs) -- direct cycle
                     -- or cycle in some root-replacement:
                  || any (\rrf -> maybe True
                                        (\fs -> rrf  `elem` (fst fs))
                                        (lookupProgInfo rrf rrinfo))
                         (fst rrfuncs))
        (lookupProgInfo qf rrinfo)

------------------------------------------------------------------------------
