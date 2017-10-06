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

test_Html_code = htmlTest1 "Joe" -=- htmlDoc1
