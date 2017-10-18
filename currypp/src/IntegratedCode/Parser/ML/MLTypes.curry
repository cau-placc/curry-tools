------------------------------------------------------------------------------
--- Types for Markup Language Parsing.
---
--- @author Max Deppert
--- @version March 2014
------------------------------------------------------------------------------
module MLTypes where

import ParseTypes
import Char

data L = X | H

data WarnID = TagNameFirstDigit
            | TagNameNotAlphaNum
            | TagEndsUnexpected
            | UnquotedAttributeEmpty
            | Unquoted Char
            | AttributesUnseperated
            | UnexpectedEndTag
            | SingleEndTag

--- A text element can be a raw text, a Curry expression that evaluates to some
--- text, or a Curry expression that evaluates to some content.
data Text = Raw String | ExpT String | ExpC String
 deriving Eq

type TPos = (SimplePos,Int)
type Symbol = (Token,TPos)
type Attribute = (String,[Text])

type Stack a = [a]
type ParseStack = Stack (Symbol,[Tree])

data Token = Break
           | Tabs Int
           | Blanks Int
           | Data [Text]
           | StartTag String [Attribute] Int
           | VoidTag String [Attribute]
           | EndTag String

data Node = Content [Text]
          | Element String [Attribute]

data Tree = Tree Node [Tree]

-- the row of a position
row :: TPos -> Int
row = fst . fst

-- the column of a position
col :: TPos -> Int
col = snd . fst

-- the number of all previous HT's in the line of a position
tbs :: TPos -> Int
tbs = snd

-- the tab weighted column of a position
wcol :: TPos -> Int
wcol p = col p + 7 * tbs p

-- the token of a symbol
tok :: Symbol -> Token
tok = fst

-- the position of a symbol
pos :: Symbol -> TPos
pos = snd

-- the tag name of a tag
tgn :: Symbol -> String
tgn (StartTag s _ _,_) = map toLower s
tgn (VoidTag s _,_) = map toLower s
tgn (EndTag s,_) = map toLower s

-- the indentation of a StartTag
ind :: Symbol -> Int
ind (StartTag _ _ i,_) = i

isTag :: Symbol -> Bool
isTag sym = isStartTag sym || isVoidTag sym || isEndTag sym

isStartTag :: Symbol -> Bool
isStartTag sym = case tok sym of
                      StartTag _ _ _ -> True
                      _              -> False

isVoidTag :: Symbol -> Bool
isVoidTag sym = case tok sym of
                     VoidTag _ _ -> True
                     _           -> False

isEndTag :: Symbol -> Bool
isEndTag sym = case tok sym of
                    EndTag _ -> True
                    _        -> False

isAlign :: Symbol -> Bool
isAlign sym = case tok sym of
                   Break    -> True
                   Tabs _   -> True
                   Blanks _ -> True
                   _        -> False

isPlain :: Symbol -> Bool
isPlain sym = case tok sym of
                   Data _ -> True
                   _      -> isAlign sym

-- stack operations

-- push an element to the top of a stack
push :: a -> Stack a -> Stack a
push = (:)

-- get the top element of a stack
top :: Stack a -> a
top = head

-- get the top element and remove it from a stack
pop :: Stack a -> Stack a
pop = tail

-- map the top element of a stack
-- minimal stack size: 1
update :: (a -> a) -> Stack a -> Stack a
update f (x:xs) = (f x) : xs

-- convert a Symbol to a Node
sym2node :: Symbol -> Node
sym2node x = case tok x of
                  Break          -> Content [Raw "\n"]
                  Tabs n         -> Content [Raw (tabs n)]
                  Blanks n       -> Content [Raw (blanks n)]
                  Data ds        -> Content ds
                  VoidTag s a    -> Element s a
                  StartTag s a _ -> Element s a
  where blanks :: Int -> String
        blanks = flip take $ repeat ' '
        tabs :: Int -> String
        tabs = flip take $ repeat (chr 9)
