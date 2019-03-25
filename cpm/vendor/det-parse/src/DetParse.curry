module DetParse where

import Prelude hiding ((<$>))

--- A parser
type Parser a = String -> [(a, String)]

--- Applies a parser to a string. If it succeeds, returns the result of the
--- parser. Returns `Nothing` if it does not.
parse :: Parser a -> String -> Maybe a
parse p s = case filter (null . snd) $ p s of
  ((x, _):_) -> Just x
  _          -> Nothing

--- A parser that always fails.
failure :: Parser a
failure = \_ -> []

--- A parser that consumes no input and results in the given value.
yield :: a -> Parser a
yield x = \s -> [(x, s)]

--- A parser that consumes no input and results in the unit value.
empty :: Parser ()
empty = yield ()

--- A parser that consumes an arbitrary single character.
anyChar :: Parser Char
anyChar = \s -> case s of
                  []      -> []
                  (c:cs)  -> [(c,cs)]

--- Builds a parser that succeeds if the predicate holds on the result of the
--- original parser.
check :: (a -> Bool) -> Parser a -> Parser a
check ok p = filter (ok . fst) . p

--- Builds a parser that consumes a specific character and results in the unit
--- value.
char :: Char -> Parser ()
char c = check (c==) anyChar *> yield ()

--- Builds a parser that consumes the given string and results in the unit
--- value.
word :: String -> Parser ()
word []     = empty
word (c:cs) = char c *> word cs

infixl 3 <|>, <!>

--- Builds a parser that tries both its argument parsers and results in the
--- result of the first one to succeed.
(<|>) :: Parser a -> Parser a -> Parser a
p <|> q = \s -> p s ++ q s

--- Builds a parser that tries its first argument parser and alternatively, if
--- the first one does not succeed, its second argument parser. In contrast to
--- `<|>`, this combinator does not backtrack. The second parser is applied to
--- the leftovers of the first parser. Use it if the alternatives are mutually
--- exclusive.
(<!>) :: Parser a -> Parser a -> Parser a
p <!> q = \s -> case p s of
                  [] -> q s
                  xs -> xs

infixl 4 <$>

--- Builds a parser that applies a function to the result of the original
--- parser.
(<$>) :: (a -> b) -> Parser a -> Parser b
f <$> p = map (\(x, s) -> (f x, s)) . p

infixl 4 <*>, <*, *>

--- Applies the function returned by the first parser to the result of the
--- second parser. Applies the parsers in order.
(<*>) :: Parser (a -> b) -> Parser a -> Parser b
p <*> q = \s -> [ (f x, s2) | (f, s1) <- p s,
                              (x, s2) <- q s1 ]

--- Builds a parser that applies both parsers in order and returns the result of
--- the first one.
(<*) :: Parser a -> Parser b -> Parser a
p <* q = (\x _ -> x) <$> p <*> q

--- Builds a parser that applies both parsers in order and returns the result of
--- the second one.
(*>) :: Parser a -> Parser b -> Parser b
p *> q = (\_ y -> y) <$> p <*> q

infixl 1 *>=

(*>=) :: Parser a -> (a -> Parser b) -> Parser b
p *>= f = \s -> [ (y, s2) | (x, s1) <- p s,
                            (y, s2) <- (f x) s1 ]

--- Builds a parser that will apply the original parser arbitrarily many times.
many :: Parser a -> Parser [a]
many p = some p <|> yield []

--- Builds a parser that will apply the original parser at least once.
some :: Parser a -> Parser [a]
some p = (:) <$> p <*> many p
