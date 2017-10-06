------------------------------------------------------------------------------
-- Example for CGI programming in Curry:
-- a form with a text input field and two event handlers
------------------------------------------------------------------------------

import HTML.Base

main :: IO HtmlForm
main = return $ form "QUESTION" $
           [htxt "Enter a string: ", textfield tref "", hrule,
            button "Reverse string"   revhandler,
            button "Duplicate string" duphandler,
            hrule]

 where
  tref free

  revhandler env = return $ form "Answer"
          [h1 [htxt $ "Reversed input: " ++ reverse (env tref)]]

  duphandler env = return $ form "Answer"
          [h1 [htxt $ "Duplicated input: " ++ env tref ++ env tref]]


-- Install the CGI program by:
-- curry makecgi -cpm -o ~/public_html/revdup.cgi RevDup
