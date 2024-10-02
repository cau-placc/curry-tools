# det-parse

det-parse is a library of deterministic parser combinators. It is based on the
material presented in Frank Huch's functional programming lecture at Kiel 
University. To use it, you build a `Parser a` using the provided combinators and 
then apply it to a string using `parse`. The simplest parsers are provided by 
the primitives `yield`, `failure`, `anyChar` and `check`.

`yield` always results in the given value, consuming no input. `yield 1` will
successfully parse the empty string to the value `1`. `failure` is a parser that 
always fails. `anyChar` is a parser that consumes a single character and uses it 
as the parse result. `check` takes a parser and a predicate on the result type 
of the parser. From these, it builds a new parser that applies the existing 
parser and succeeds only if the predicate holds for the parse result.

`char` and `word` build parsers for single characters and whole strings from
these primitives. `char 'c'` is a parser that consumes the single character `c`
and results in the unit value `()`. `word "hello"` consumes the string `hello`
and results in the unit value. `empty` is a parser that recognizes an empty 
string and results in the unit value.

The operators `*>` and `<*` are provided to combine parsers into more comples
ones. `*>` applies two parsers and returns the result of the second one if both
were successful. `char 'a' *> yield 1` is successful if applied to the string 
`a` and results in the value `1`. `<*` applies two parsers in the same order, 
i.e. left to right, but returns the result of the first one.

`<|>` combines two parsers by applying them both. If the first one is 
successful, it returns its result. If it is not, but the second one is, then it
returns the result of the second one. If both are unsuccessful, the combined
parser is unsuccessful as well. The parser 
`char 'a' *> yield 1 <|> char 'b' *> yield 2` parses the string `a` to
the value `1` and the string `b` to the value `2`. `<!>` works similarly to 
`<|>`, but does not backtrack. That is, it only tries the second parser if the
first one was unsuccessful, and only on the remaining input. It can be used if 
the alternatives do not overlap. The above example would also work if `<|>` were 
replaced by `<!>`, while `word "ab" *> yield 1 <!> word "abc" *> yield 2` would 
fail to parse `abc` into the value `2` since the first alternative has already 
consumed `ab`.

`<$>` builds a new parser from an existing parser by applying a function to the
result of that parser. For example, `(+ 1) <$> (char 'a' *> yield 1)` is a 
parser that parses the string `a` into the value `2`.

`<*> :: Parser (a -> b) -> Parser a -> Parser b ` combines two parsers, one that 
results in a function from `a` to `b`, and one that results in an `a` value. It
applies the parsers in order and then applies the function result of the first
parser to the value result of the second parser.

`many :: Parser a -> Parser [a]` builds a parser that parses whatever the 
original parser parses arbitrarily many times. `some` is similar, but requires
that the original parser succeed at least once. Applying 
`many (char 'a' *> yield 1)` to the string `aaaa` results in the value 
`[1,1,1,1]`.
