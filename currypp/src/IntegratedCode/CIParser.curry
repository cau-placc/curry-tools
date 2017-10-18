------------------------------------------------------------------------------
--- The Parser for Curry with Integrated Code
--- =========================================
---
--- @author Jasper Sikorra - jsi@informatik.uni-kiel.de
--- @version March 2014
------------------------------------------------------------------------------
module CIParser(parse) where

import List
import Char

import ParseTypes

-- The identificators for Integrated Expressions
s_ident :: Char
s_ident = '`'

e_ident :: Char
e_ident = '\''

min_number :: Int
min_number = 2

-- Identifiers for block comments and line comments
s_block_comment :: String
s_block_comment = "{-"

e_block_comment :: String
e_block_comment = "-}"

s_line_comment :: String
s_line_comment = "--"

-- Error messages
err_missing_quote :: String
err_missing_quote = "Missing corresponding closing quote!"

err_missing_block :: String
err_missing_block = "Missing corresponding closing comment block delimiter "
                     ++ e_block_comment ++ " !"

err_missing_integ :: String
err_missing_integ = "Integrated code not terminated with correct number " ++
                    "of " ++ [e_ident] ++ " chars"

err_no_langtag    :: String
err_no_langtag    = "Missing language tag for integrated code!"

err_layout_code   :: String
err_layout_code   = "Bad layout. Check amount of white spaces!"

--- The parse function is the main function of the Code Integration Parser.
--- The functions partitions the input in normal code and integrated
--- expressions, disassembles the integrated code and removes its offset.
--- @param filename - The filename of the input file
--- @param input    - The input string containing language with integrated code
--- @return   - A list of StandardTokens which contain either the common
---             language or the DSL code with some extra information
parse :: Filename -> String -> IO (PM [StandardToken])
parse fn input = return $ bindPM (parserL1 (initPos fn) input) parserL2

{- FIRST LEVEL OF THE PARSER
   The first level of the parser starts here. It recognizes the
   integrated expressions which are introduced by multiple s_idents and
   terminated by the same amount of e_idents.
-}
data L1Token = Normal Pos     String
             | Exp    Pos                -- Integrated Expression with Postion
                      Int                -- number of start identifiers
                      String             -- content

--- Function for the first level of the parser
--- which recognizes integrated expressions and normal Curry expressions
--- @param p - The starting position of the expression
--- @param s - The input
parserL1 :: Pos -> String -> PM [L1Token]
parserL1 p s = parserL1Iter "" p p s

parserL1Iter :: String -> Pos -> Pos -> String -> PM [L1Token]
parserL1Iter acc accP _ "" =
  cleanPM (if_then_else (null acc) [] [Normal accP (reverse acc)])
parserL1Iter acc accP p s@(c:cs)
  -- Parse Quotations
  | c == '\"' = passThrough parseQuotation (movePosByChar p '\"') cs
  -- Parse Quotations with single quotes
  | c == '\'' = passThrough parseSingleQuote (movePosByChar p '\'') cs
  | otherwise =
  -- Recognize line comments
    let (isprefco,rstco) = isPrefixAndDrop s_line_comment s
    in  if isprefco
          -- Parse line comments
          then passThrough parseLineComment (movePosByString p s_line_comment)
                 rstco
          else
            -- Recognize block comments
            let (isprefbc,rstbc) = isPrefixAndDrop s_block_comment s
            in if isprefbc
              -- Parse block comments
              then passThrough parseBlockComment
                     (movePosByString p s_block_comment) rstbc
              else
                -- Recognize integrated expressions
                let (n,r) = countAndDrop s_ident s
                in if (min_number <= n)
                    -- Parse integrated expressions
                    then passThroughInt parseIntegrated
                               (movePosByString p (replicate n s_ident)) n r
                    -- Parse common code
                    else  parserL1Iter (c:acc) accP (movePosByChar p c) cs
  where
    passThrough f np st =
      if (null acc) then f p np st
        else liftPM ((:) (Normal accP (reverse acc))) $ f p np st
    passThroughInt f np i st =
      if (null acc) then f p np i st
        else liftPM ((:) (Normal accP (reverse acc))) $ f p np i st

--- The function isPrefixAndDrop checks wether a list is a prefix of
--- another and drops the prefix
--- @l1 - The possible prefix
--- @l2 - The list
--- @return (b,l) where b states wether l1 is a prefix of l2 and
---         l is the remaining list
isPrefixAndDrop :: Eq a => [a] -> [a] -> (Bool,[a])
isPrefixAndDrop [] []                     = (True,[])
isPrefixAndDrop (_:_) []                  = (False,[])
isPrefixAndDrop [] (c:cs)                 = (True,(c:cs))
isPrefixAndDrop (c:cs) (d:ds) | c == d    = isPrefixAndDrop cs ds
                              | otherwise = (False,(d:ds))

--- The function countAndDrop drops all elements of one kind
--- from the beginning of the list, counts how many elements are
--- dropped and returns the remaining list
--- @param c - The element which should be dropped
--- @param l - The list
--- @return (i,l) where i states the amount of dropped elements and
---         l is the remaining list
countAndDrop :: Eq a => a -> [a] -> (Int,[a])
countAndDrop c s = countAndDropIter 0 s
  where
    countAndDropIter n  []                 = (n,[])
    countAndDropIter n (c1:cs) | c == c1   = countAndDropIter (n+1) cs
                               | otherwise = (n,(c1:cs))
--- The function parseQuotation parses a quotation if a '\"' is already found
--- @param pos1 - The starting position before the already found '\"'
--- @param pos2 - The starting position after the already found '\"'
--- @param s    - The input
--- @return tok - The parsed quote and the parsed rest of input
parseQuotation :: Pos -> Pos -> String -> PM [L1Token]
parseQuotation accP p s =
  let (qu,rst,fnd,pos) = findUnescapedChar p '\"' '\\' s
  in if fnd then liftPM ((:) (Normal accP ('\"':qu))) $ parserL1 pos rst
       else throwPM accP err_missing_quote

--- The function findUnescapedChar finds a specific Char, which is not
--- introduces by another Char in a String
--- @param pos - The starting position of the string
--- @param c1  - The Char which should be searched
--- @param c2  - The Char which escapes c1
--- @param s   - The input String
--- @return (s1,s2,b,p) - s1 is the input before c1, s2 is the input after
---         c1, b states wether c1 was found, pos points to the start of s2
findUnescapedChar :: Pos -> Char -> Char -> String -> (String,String,Bool,Pos)
findUnescapedChar pos ch escaper s = findUnescapedCharHelper pos "" s
  where
    findUnescapedCharHelper p acc ""     = (reverse acc,"",False,p)
    findUnescapedCharHelper p acc (c:[])
      | c == ch = (reverse (c:acc),[],True,movePosByChar p c)
      | c /= ch = (reverse (c:acc),[],False,movePosByChar p c)
    findUnescapedCharHelper p acc (c:d:ds)
      | c == ch                 =
        (reverse (c:acc),(d:ds),True,movePosByChar p c)
      | c /= ch && c == escaper =
        findUnescapedCharHelper (movePosByString p [d,c]) (d:c:acc) ds
      | otherwise               =
        findUnescapedCharHelper (movePosByChar p c) (c:acc) (d:ds)

--- The function parseSingleQuote parses a single quote, but only if
--- it escapes a quote or starting identifiers for integrated expressions.
--- @param p1 - The staring position of the single quote
--- @param p2 - The position after the first single quote
--- @param s  - The input
--- @return   - The parse result of the complete input
parseSingleQuote :: Pos -> Pos -> String -> PM [L1Token]
parseSingleQuote accP _ ""          = cleanPM [Normal accP "\'"]
parseSingleQuote accP p s@(c:_)  =
  case s of
    ('\\':d:'\'':rst) ->
      liftPM ((:) (Normal accP ['\'','\\',d,'\'']))
        $ parserL1 (moveAbs (moveCol p 4) 4) rst
    (_:'\'':rst)      ->
      liftPM ((:) (Normal accP ['\'',c,'\'']))
        $ parserL1 (moveAbs (moveCol p 3) 3) rst
    _                -> liftPM ((:) (Normal accP "\'")) $ parserL1 p s

--- The function parseLineComment parses a line comment introduced
--- by s_line_comment
--- @param p1 - The position before the line comment
--- @param p2 - The position after s_line_comment
--- @param s  - The input
--- @return   - The parsed input
parseLineComment :: Pos -> Pos -> String -> PM [L1Token]
parseLineComment accP p s =
  let (co,rst,fnd,pos) = findSequence p "\n" s
  in  if fnd
        then liftPM ((:) (Normal accP (s_line_comment ++ co)))
                $ parserL1 pos rst
        else cleanPM [Normal accP (s_line_comment ++ co)]

--- The function parseBlockComment parses a block comment introduced
--- by s_block_comment and terminated by e_block_comment
--- @param p1 - The position before the block comment
--- @param p2 - The position after s_block_comment
--- @param s  - The input
--- @return   - The parsed output
parseBlockComment :: Pos -> Pos -> String -> PM [L1Token]
parseBlockComment accP p s =
  let (blc,rst,fnd,pos) = findSequence p e_block_comment s
  in  if fnd
        then liftPM ((:) (Normal accP (s_block_comment ++ blc)))
               $ parserL1 pos rst
        else throwPM accP err_missing_block

--- The function findSequence finds a sequence of characters in a string,
--- and divides the string after the sequence
--- @param p   - The position at the start of the input
--- @param seq - The sequence which should be found
--- @param s   - The input string
--- @return (s1,s2,b,p) - s1 is the string before and with the seq
---                       s2 is the string after the seq
---                       b  states wether seq was found
---                       p is the position of s2
findSequence :: Pos -> String -> String -> (String,String,Bool,Pos)
findSequence p1 s1 s2 = findSequenceIter p1 [] s1 s2
  where
    findSequenceIter p acc [] []     = (reverse acc,[],True,p)
    findSequenceIter p acc [] (c:cs) =
      (reverse acc,(c:cs),True,p)
    findSequenceIter p acc (_:_) []  = (reverse acc,[],False,p)
    findSequenceIter p acc (c:cs) (d:ds)
      | c == d    = findSequenceIter (movePosByChar p d) (d:acc) cs ds
      | otherwise = findSequenceIter (movePosByChar p d) (d:acc) s1 ds

--- The function parseIntegrated parses integrated expressions
--- @param p1 - The position at the start of the integrated expression
--- @param p2 - The position after all s_ident(s) introduction the exp.
--- @param n  - The number of s_ident(s) that introduced the exp.
--- @param s  - The input
--- @return   The parsed input
parseIntegrated :: Pos -> Pos -> Int -> String -> PM [L1Token]
parseIntegrated accP p n s =
  let (exp,rst,fnd,pos) = findReplicate p n e_ident s_ident s
  in if fnd then liftPM ((:) (Exp accP n exp))$ parserL1 pos rst
      else throwPM accP err_missing_integ

--- The function findReplicate finds sequences of the same character in a
--- string. This function is defined to find such sequences for two
--- different characters.
--- @param pos - The position at the start of the input
--- @param n   - The amount of same character in a row which should be searched
--- @param c1  - The first character of which sequence should be searched
--- @param c2  - The second character of which sequence should be searched
--- @param s   - The input
--- @return (s1,s2,b,p) - s1 is the input before and with the sequence
---                       s2 is the input after the sequence
---                       b  states wether a sequence of c1 was found
---                       p  points to the position of s2
findReplicate :: Pos -> Int -> Char -> Char -> String -> (String,String,Bool,Pos)
findReplicate p1 n e_i s_i s = findReplicateIter p1 [] 0 s
  where
    findReplicateIter p accS accN []      =
      if (accN == (-n)) then (s,s,False,p1)
        else (reverse (drop accN accS),[],(accN == n),p)
    findReplicateIter p accS accN (c2:cs) =
      if (accN == n) then (reverse (drop accN accS),(c2:cs),True,p)
        else
          if (accN == (-n))
            then (s,s,False,p1)
          else
            if (e_i == c2)
              then
                if (accN < 0)
                  then findReplicateIter (movePosByChar p c2)(c2:accS) 1 cs
                  else
                    findReplicateIter (movePosByChar p c2)(c2:accS) (accN+1) cs
            else
              if (s_i == c2)
                then
                  if (accN < 0)
                    then findReplicateIter
                      (movePosByChar p c2) (c2:accS) (accN-1) cs
                    else findReplicateIter
                      (movePosByChar p c2) (c2:accS) (-1) cs
                else findReplicateIter (movePosByChar p c2) (c2:accS) 0 cs

{- SECOND LEVEL OF THE PARSER
   The second level of the parser starts here. The integrated expressions
   are disassembled and converted to StandardTokens
-}
--- The function parserL2 converts Normals to StandardTokens and
--- disassembles the Exps and converts them to StandardTokens
--- @param input - A list of tokens from parser level 1
--- @result A list of StandardTokens utilizeable in the Translator
parserL2 :: [L1Token] -> PM [StandardToken]
parserL2 []                  = cleanPM []
parserL2 (t:tks) | isNormal t = liftPM ((:) $ normalToStTk t) $ parserL2 tks
                 | otherwise  =
  bindPM (disassembleIntExp t) (\ptk -> liftPM ((:) ptk) $ parserL2 tks)

isNormal :: L1Token -> Bool
isNormal t =
  case t of
    (Normal _ _) -> True
    _            -> False

--- normalToStTk converts Normal(s) to StandardToken(s)
--- @param normal - A Normal L1Token
--- @result       - An equivalent StandardToken
normalToStTk :: L1Token -> StandardToken
normalToStTk (Normal p s) = StTk p p Nothing s
normalToStTk (Exp _ _ _)  = failed

--- disassembleIntExp disassembles Exps, removes the offset from the
--- DSL code and converts the outcome to StandardToken
--- @param exp - The L1Token Exp which should be converted
--- @return standardtk - A StandardToken equivalent to the exp but with removed
---                      offset in the DSL code
disassembleIntExp :: L1Token -> PM StandardToken
disassembleIntExp (Exp p i s) =
    let
        -- Recognize the language tag
        (langtag,rest1) = break isSpace s
        -- Recognize the whitespaces and the dsl
        (spaces,dsl) = span isSpace rest1
    in
        if (null langtag) then throwPM p err_no_langtag
            else
              let
                -- calculate position of the DSL code
                posBeforeDSL = movePosByString p
                 ((replicate i s_ident) ++ langtag ++ spaces)
                -- calculate the offset
                offset = getCol posBeforeDSL
                -- remove the offset from the DSL
                cleanDSL = removeOffset posBeforeDSL offset dsl
              in bindPM cleanDSL
                   (\cDSL -> cleanPM (StTk p posBeforeDSL (Just langtag) cDSL))
disassembleIntExp (Normal _ _) = failed

--- removeOffset removes the offset from a string
--- @param n - Length of the offset
--- @param s - The input
--- @return  - The input with removed offset
removeOffset :: Pos -> Int -> String -> PM String
removeOffset p n s =
    let linesOfDSL = lines s
    in  case linesOfDSL of
      []      -> cleanPM ""
      [st]    -> cleanPM st
      (x:xs) -> liftPM ((++) x) $ removeOffsetFromLines
                                    (movePosByString p (x ++ "\n")) n xs

--- removeOffsetFromLines removes an offset of a certain length from
--- each line of a string
--- @param p - Position of the start of the input
--- @param n - Length of the offset
--- @param s - Input
--- @return  - The input with removed offset wrapped in the ParserMonad
removeOffsetFromLines :: Pos -> Int -> [String] -> PM String
removeOffsetFromLines _ _ []    = cleanPM ""
removeOffsetFromLines p n (l:ls) =
  let rmoffl = removeOffsetFromLine p n l
  in bindPM rmoffl (\(np,s) -> liftPM ((++) ('\n':s)) $
                                removeOffsetFromLines
                                  (movePosByChar np '\n') n ls)

--- removeOffsetFromLine is a helper function for removeOffsetFromLines
--- @param p - Position pointing to the start of the line
--- @param n - The length of the offset
--- @param s - The input line
--- @return  - The input with removed offset wrapped in the ParserMonad
removeOffsetFromLine :: Pos -> Int -> String -> PM (Pos,String)
removeOffsetFromLine pos i st = removeOffsetFromLineIter pos st
  where
    removeOffsetFromLineIter p s =
      if (getCol p >= i) then cleanPM (p,s)
        else case s of
         []     -> cleanPM (p,"")   -- The line is empty
         (c:cs) -> if (isWhiteSpace c)
                     then removeOffsetFromLineIter (movePosByChar p c) cs
                     else throwPM pos err_layout_code

--- isWhiteSpace is similar to isSpace function but without newlines
isWhiteSpace :: Char -> Bool
isWhiteSpace c = c == ' ' || c == '\t'
