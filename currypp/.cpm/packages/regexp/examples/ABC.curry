---------------------------------------------------------------------
-- Examples and tests for regular expressions semantics and matching.
---------------------------------------------------------------------

import RegExpSem
import Test.Prop

-- Example: regular expression (a|b|c)
abc :: RE Char
abc = Alt (Alt (Lit 'a') (Lit 'b')) (Lit 'c')

-- Example: regular expression (ab*)
abstar :: RE Char
abstar = Conc (Lit 'a') (Star (Lit 'b'))

-- Example: regular expression (ab+)
abplus :: RE Char
abplus = Conc (Lit 'a') (plus (Lit 'b'))


test1 = sem abc <~> ("a" ? "b" ? "c")

test2 = grep abc    "dbe"   <~> True

-- The following evaluation is finite due to demand-driven non-determinism:
test3 = grep abstar "dabe"  <~> True

test4 = grepShow abstar "dbabe"       <~> "abe"

test5 = grepShow abc    "dbeaxce"     <~> ("beaxce" ? "axce" ? "ce")

test6 = grepPos abplus "sfsfsfabdff"  <~> 6
