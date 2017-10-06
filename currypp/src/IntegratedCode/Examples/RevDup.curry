{-# OPTIONS_CYMAKE -F --pgmF=currypp --optF=foreigncode #-}

------------------------------------------------------------------------------
-- Example for CGI programming in Curry:
-- a form with a text input field and two event handlers
-- to reverse or duplicate the input.
------------------------------------------------------------------------------

import HTML.Base

{-
Remark:

The integrated code ``html ...'' expands to a list of HTML expressions,
i.e., to an expression of type [HtmlExp].

A Curry expression enclosed with {...} inside the integrated code
must be of type String and is inserted as (HTML-quoted) text.

A Curry expression enclosed with {{...}} inside the integrated code
must be of type HtmlExp and is inserted as an HTML element.
-}

main :: IO HtmlForm
main = return $ form "Question" $
         ``html
	     Enter a string: {{textfield tref ""}}
	     <hr>
             {{button "Reverse string"   revhandler}}
             {{button "Duplicate string" duphandler}}''

 where
  tref free

  revhandler env = return $ form "Answer"
    ``html <h1>Reversed input: {reverse (env tref)}''

  duphandler env = return $ form "Answer"
    ``html
       <h1>
         Duplicated input:
         {env tref ++ env tref}''


-- Install the CGI program by:
-- curry makecgi -cpm -o ~/public_html/revdup.cgi RevDup
