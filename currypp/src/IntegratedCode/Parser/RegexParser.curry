------------------------------------------------------------------------------
--- A Regex Parser
---
--- @author Jasper Sikorra
--- @version July 2017
------------------------------------------------------------------------------

{-# OPTIONS_CYMAKE -Wno-missing-signatures -Wno-incomplete-patterns #-}

module RegexParser(parse) where

import Parser
import Char
import List
import ReadNumeric

import ParseTypes

--- The parse function allows the translation of extended regular expression
--- to normal Curry code.
--- @param po - The position of the ERE code
--- @param st - The ERE code
--- @return A string containg normal curry code with the same semantics as the
---         original ERE code
parse :: LangParser
parse po st = return (liftPM (\p -> "(" ++ showRegex p ++ ")")
                             (parsen po (lex st)))

--- The function showRegex is used to generate a string containing the
--- target code
showRegex :: Regex -> String
showRegex []     = "[]"
showRegex re@(_:_) = "[" ++ (init $ showRegexHelper re) ++ "]"

showRegexHelper :: Regex -> String
showRegexHelper []     = ""
showRegexHelper (r:rs) = case r of
    Literal s     -> "Literal (" ++ s ++ ")," ++ showRegexHelper rs
    Xor x1 x2     -> "Xor (" ++ showRegex x1 ++ ") (" ++ showRegex x2 ++ "),"
                      ++ showRegexHelper rs
    Star x        -> "Star (" ++ showRegex x ++ "),"  ++ showRegexHelper rs
    Plus x        -> "Plus (" ++ showRegex x ++ "),"  ++ showRegexHelper rs
    AnyLiteral    -> "AnyLiteral,"  ++ showRegexHelper rs
    Bracket x     -> "Bracket " ++ showEither x ++ "," ++ showRegexHelper rs
    NegBracket x  -> "NegBracket " ++ showEither x ++ "," ++ showRegexHelper rs
    Start x       -> "Start (" ++ showRegex x ++ "),"  ++ showRegexHelper rs
    End x         -> "End (" ++ showRegex x ++ ")," ++ showRegexHelper rs
    Times (i,j) x -> "Times (" ++ show i ++ "," ++ show j ++ ") ("
                     ++ showRegex x ++ ")," ++ showRegexHelper rs
    _             -> show r ++ "," ++ showRegexHelper rs
  where
    showEither :: [Either String (String,String)] -> String
    showEither []     = ""
    showEither (x:xs) = showE "" (x:xs)
        where
          showE acc []     = "[" ++ (init acc) ++ "]"
          showE acc (y:ys) = case y of
            (Left s)        -> showE (acc ++ "Left (" ++ s ++ "),") ys
            (Right (s1,s2)) -> showE (acc ++ "Right " ++ "((" ++ s1 ++ "),("
                                     ++ s2 ++ "))" ++ ",") ys

--- An intermediate data type which can be used for code generation
type Regex = [ORegex]

data ORegex = Nil
           | Literal String
           | Xor Regex Regex
           | Star Regex
           | Plus Regex
           | AnyLiteral
           | Bracket [Either String (String,String)]
           | NegBracket [Either String (String,String)]
           | Start Regex
           | End Regex
           | Times (Int,Int) Regex
 deriving Show

--- Possible regex operators
operators       = ['|','*','.','[',']','^','$','{','}','(',')','?','+']
--- Characters which can be used to escape other characters
escapers        = ['\\']
--- Characters which aren't operators but still escapable
non_op_escp     = ['<','>','\n','\t','\\','-']
--- All characters which are escapable
escapable       = flip elem (non_op_escp ++ operators)
--- POSIX classes describing ranges
posixclasses    = [":alnum:",":alpha:",":blank:",":cntrl:",":digit:",
                   ":graph:",":lower:",":print:",":punct:",":space:",
                   ":upper:",":xdigit:"]

--- Converting POSIX classes to square bracket expressions
posixclassconv :: String -> [Either String (String,String)]
posixclassconv =  map eccToEss . posixclasscon
  where
    eccToEss :: Either Char (Char,Char) -> Either String (String,String)
    eccToEss (Left c) = Left (show c)
    eccToEss (Right (c1,c2)) = Right (show c1,show c2)

posixclasscon :: String -> [Either Char (Char,Char)]
posixclasscon str = case str of
  ":alnum:"   ->  [Right ('A','Z'), Right ('a','z'), Right ('0','9')]
  ":alpha:"   ->  [Right ('A','Z'), Right ('a','z')]
  ":blank:"   ->  [Left ' ', Left '\t']
  ":cntrl:"   ->  [Right ('\x00','\x1F'), Left '\x7F']
  ":digit:"   ->  [Right ('0','9')]
  ":graph:"   ->  [Right ('\x21','\x7E')]
  ":lower:"   ->  [Right ('a','z')]
  ":print:"   ->  [Right ('\x20','\x7E')]
  ":punct:"   ->  [Left ']', Left '[', Left '!', Left '"',  Left '#',
                   Left '$', Left '%', Left '&', Left '\'', Left '(',
                   Left ')', Left '*', Left '+', Left ',' , Left '.',
                   Left '/', Left ':', Left ';', Left '<' , Left '=',
                   Left '>', Left '?', Left '@', Left '\\', Left '^',
                   Left '_', Left '`', Left '{', Left '|',  Left '}',
                   Left '~', Left '-']
  ":space:"   ->  [Left ' ', Left '\t', Left '\r', Left '\n',
                             Left '\v', Left '\f']
  ":upper:"   ->  [Right ('A','Z')]
  ":xdigit:"  ->  [Right ('A','F'), Right ('a','f'), Right ('0','9')]

--- Tokens for the lexer
data Token = TokenStar
           | TokenBar
           | TokenPoint
           | TokenDash
           | TokenDollar
           | TokenPlus
           | TokenOSBracket
           | TokenCSBracket
           | TokenORBracket
           | TokenCRBracket
           | TokenOCBracket
           | TokenCCBracket
           | TokenOABracket
           | TokenCABracket
           | TokenLiteral Char
 deriving Eq

--- Assigning Tokens to Chars
tokenToChar :: Token -> Char
tokenToChar t = case t of
  TokenStar       -> '*'
  TokenBar        -> '|'
  TokenPoint      -> '.'
  TokenDash       -> '^'
  TokenDollar     -> '$'
  TokenPlus       -> '+'
  TokenOSBracket  -> '['
  TokenCSBracket  -> ']'
  TokenORBracket  -> '('
  TokenCRBracket  -> ')'
  TokenOCBracket  -> '{'
  TokenCCBracket  -> '}'
  TokenOABracket  -> '<'
  TokenCABracket  -> '>'
  TokenLiteral c  ->  c

--- Lexer
lex :: String -> [Token]
lex ""  = []
lex str@(c:cs) = case str of
  ('*':_)    -> (TokenStar         :lex cs)
  ('|':_)    -> (TokenBar          :lex cs)
  ('.':_)    -> (TokenPoint        :lex cs)
  ('^':_)    -> (TokenDash         :lex cs)
  ('$':_)    -> (TokenDollar       :lex cs)
  ('?':_)    -> lex ('{':'0':',':'1':'}':cs)
  ('+':_)    -> (TokenPlus         :lex cs)
  ('[':_)    -> (TokenOSBracket    :lex cs)
  (']':_)    -> (TokenCSBracket    :lex cs)
  ('(':_)    -> (TokenORBracket    :lex cs)
  (')':_)    -> (TokenCRBracket    :lex cs)
  ('{':_)    -> (TokenOCBracket    :lex cs)
  ('}':_)    -> (TokenCCBracket    :lex cs)
  ('<':_)    -> (TokenOABracket    :lex cs)
  ('>':_)    -> (TokenCABracket    :lex cs)
  ('-':_)    -> (TokenLiteral '-'  :lex cs)
  ('\\':d:ds) -> if (escapable d) then
                    case d of
                      'n'       -> TokenLiteral '\n':lex ds
                      't'       -> TokenLiteral '\t':lex ds
                      _         -> TokenLiteral d   :lex ds
                  else (TokenLiteral '\\':lex cs)
  ('\n':_)   -> lex cs
  _           -> (TokenLiteral c    :lex cs)

--- Parser
parsen :: Pos -> [Token] -> PM Regex
parsen p tks = pars p (cleanPM []) tks

pars :: Pos -> PM Regex -> [Token] -> PM Regex
pars _ prr []     = prr
pars p prr (t:ts) = bindPM prr (\r ->
  case t of
      TokenStar        -> liftPM ((:) (Star r)) (parsen p ts)
      TokenBar         -> parseBar p r ts
      TokenPoint       -> liftPM ((++) r) (pars p (cleanPM [AnyLiteral]) ts)
      TokenDash        -> liftPM ((++) r) (parseDash p ts)
      TokenDollar      -> liftPM ((++) r) (parseDollar p ts)
      TokenPlus        -> liftPM ((:) (Plus r)) (parsen p ts)
      TokenOSBracket   -> liftPM ((++) r) (parseOSBracket p ts)
      TokenCSBracket   -> throwPM p "No \'[\' for \']\' found"
      TokenORBracket   -> liftPM ((++) r) (parseRBracket p ts)
      TokenCRBracket   -> throwPM p "No \'(\' for \')\' found"
      TokenOCBracket   -> parseCBracket p r ts
      TokenCCBracket   -> throwPM p "No \'{\' for \'}\' found"
      TokenOABracket   -> let prsrs = parseABracket p ts
                              prs   = fstPM prsrs
                              prrs  = sndPM prsrs
                           in bindPM prs (\s -> bindPM prrs (\rs -> liftPM
                                  ((++) r) (pars p (cleanPM [Literal s]) rs)))
      TokenCABracket   -> throwPM p "No \'<\' for \'>\' found"
      TokenLiteral c   -> liftPM ((++) r)
                                 (pars p (cleanPM [Literal (show c)]) ts))

--- Alternative
parseBar :: Pos -> Regex -> [Token] -> PM Regex
parseBar p r ts = liftPM (\x -> [Xor r x]) (parsen p ts)

--- Start
parseDash :: Pos -> [Token] -> PM Regex
parseDash p ts = liftPM (\li -> Start [head li]:tail li) (parsen p ts)

--- End
parseDollar :: Pos -> [Token] -> PM Regex
parseDollar p ts = liftPM (\li -> End [head li]:tail li) (parsen p ts)

--- Square Bracket (Range)
parseOSBracket :: Pos -> [Token] -> PM Regex
parseOSBracket p []     = throwPM p "Missing ']'"
parseOSBracket p (t:ts) = case t of
  TokenDash -> let (cont,rst) = case ts of
                    -- Handle closing square bracket directly after opening,
                    -- meaning the ']' char is an option
                    (TokenCSBracket:tss) ->
                      ((\(x,y) -> ((TokenCSBracket:x),y))
                       (span (\x -> x /= TokenCSBracket) tss))
                    _            -> span (\x -> x /= TokenCSBracket) ts
               in pars p (liftPM (\x -> [NegBracket x]) (squareParser p cont))
                         (tail rst)
  _         -> let (cont,rst) = case (t:ts) of
                    -- Handle closing square bracket directly after opening,
                    -- meaning the ']' char is an option
                    (TokenCSBracket:tss) ->
                      (\(x,y) -> ((TokenCSBracket:x),y))
                        (span (\x -> x /= TokenCSBracket) tss)
                    _           -> span (\x -> x /= TokenCSBracket)
                                                (t:ts)
               in pars p (liftPM (\x -> [Bracket x]) (squareParser p cont))
                         (tail rst)

squareParser :: Pos -> [Token] -> PM [Either String (String,String)]
squareParser pos toks =
      let chars  = extractChars toks
      in if (elem chars posixclasses)
          then cleanPM (posixclassconv chars)
          else rangeAndCharParser pos toks
  where
    rangeAndCharParser p tks = case tks of
      []
        -> cleanPM []
      (TokenLiteral a:TokenLiteral '-':TokenLiteral b:ts)
        -> liftPM (\rc -> ((:) (Right (show a,show b))) rc)
                  (rangeAndCharParser p ts)
      (TokenOABracket:ts)
        -> let prsrs = parseABracket p ts
               prs   = fstPM prsrs
               prrs  = sndPM prsrs
            in (bindPM prs $  \s  ->
                bindPM prrs $ \rs ->
              if (head rs == TokenLiteral '-')
                then
                  if (head (tail rs) == TokenOABracket)
                    then
                      let prs2rs2 = parseABracket p (tail (tail rs))
                          prs2    = fstPM prs2rs2
                          prrs2   = sndPM prs2rs2
                      in (bindPM prs2 $  \s2  ->
                          bindPM prrs2 $ \rs2 ->
                          liftPM ((:) (Right (s,s2))) $
                            rangeAndCharParser p rs2)
                    else liftPM ((:)
                            (Right (s,(\(TokenLiteral c) -> (show c))
                                          (head (tail rs)))))
                            (rangeAndCharParser p (tail (tail rs)))
                else liftPM ((:) (Left s)) (rangeAndCharParser p rs))
      _
        -> liftPM ((:) (Left (show $ tokenToChar $ head tks)))
                  (rangeAndCharParser p (tail tks))

--- Round bracket (Paranthesis)
parseRBracket :: Pos -> [Token] -> PM Regex
parseRBracket p ts = pars p (parsen p (init cont)) rst
  where
    (cont,rst) = splitAt (cntUntilClosed TokenORBracket TokenCRBracket ts) ts

--- Curly bracket (Multiple times)
parseCBracket :: Pos -> Regex -> [Token] -> PM Regex
parseCBracket p r ts = pars p (cleanPM [(Times (curlyParser (init cont)) r)])
                              rst
  where
    (cont,rst) = splitAt (cntUntilClosed TokenOCBracket TokenCCBracket ts) ts

curlyParser :: [Token] -> (Int,Int)
curlyParser tks = (fst fir,fst sec)
  where
    fir = maybe failed id (readNat (extractChars tks))
    sec = maybe failed id (readNat (tail (snd fir)))

--- Arrow bracket (Variable)
parseABracket :: Pos -> [Token] -> PM (String,[Token])
parseABracket = pOAB []
  where
    pOAB _ p []       = throwPM p "Missing '>'"
    pOAB acc p (t:ts) = case t of
      TokenCABracket   -> cleanPM (reverse acc,ts)
      _                -> pOAB (tokenToChar t:acc) p ts

--- Extract a char from a token (necessary for variables)
extractChars :: [Token] -> String
extractChars []     = ""
extractChars (t:ts) = case t of
  (TokenLiteral c) -> (c:extractChars ts)
  _                -> ""

-- | Helper
cntUntilClosed :: Eq a => a -> a -> [a] -> Int
cntUntilClosed c1 c2 li = cUCB 0 0 li
  where
    cUCB n c l =
      if (c < 0) then n else
        if (l == []) then failed else
          if ((head l) == c1) then cUCB (n+1) (c+1) (tail l) else
            if ((head l) == c2) then cUCB (n+1) (c-1) (tail l)
              else cUCB (n+1) c (tail l)
