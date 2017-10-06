{-# OPTIONS_CYMAKE -F --pgmF=currypp --optF=foreigncode #-}

------------------------------------------------------------------------------
--- This program contains tests for integrated code to support
--- easy writing of HTML code in Curry programs.
------------------------------------------------------------------------------

import HTML.Base
import Test.EasyCheck

htmlTest1 :: String -> [HtmlExp]
htmlTest1 name = ``html
 <html>

  <head>
   <title>Simple Test

  <body>
   <h1>Hello {name}!</h1>
    <p>
     Bye!
    <p>Bye!
   <h2>{reverse name}
   Bye!''

htmlDoc1 :: [HtmlExp]
htmlDoc1 =
  [HtmlStruct "html" []
    [HtmlStruct "head" []
      [HtmlStruct "title" [] [HtmlText "Simple Test\n"]],
     HtmlStruct "body" []
      [HtmlStruct "h1" []
        [HtmlText "Hello ", HtmlText "Joe", HtmlText "!"],
       HtmlStruct "p" [] [HtmlText "Bye!\n"],
       HtmlStruct "p" [] [HtmlText "Bye!\n"],
       HtmlStruct "h2" []
        [HtmlText "eoJ", HtmlText "\n"],
       HtmlText "Bye!"]]]

------------------------------------------------------------------------------
-- Partial equality on HTML documents for testing.
instance Eq HtmlExp where
  hexp1 == hexp2 = case (hexp1,hexp2) of
    (HtmlText s, HtmlText t) -> s == t
    (HtmlStruct t ats hes, HtmlStruct t' ats' hes') ->
                                        t==t' && ats==ats' && hes == hes'
    _ -> error "HTML.==: cannot compare cgi refs or handlers"

------------------------------------------------------------------------------

test_Html_code = always (htmlTest1 "Joe" == htmlDoc1)
