------------------------------------------------------------------------------
--- Library for constructing static and dynamic HTML pages.
--- [This paper](http://www.informatik.uni-kiel.de/~mh/papers/PADL01.html)
--- contains a description of the basic ideas behind this library.
---
--- The installation of a cgi script written with this library
--- can be done by the command
---
---     curry makecgi -m initialForm -o /home/joe/public_html/prog.cgi prog
---
--- where `prog` is the name of the Curry program with
--- the cgi script, `/home/joe/public_html/prog.cgi` is
--- the desired location of the
--- compiled cgi script, and `initialForm` is the Curry expression
--- (of type IO HtmlForm) computing the HTML form (where `curry`
--- is the shell command calling the Curry system PAKCS or KiCS2).
---
--- @author Michael Hanus (with extensions by Bernd Brassel and Marco Comini)
--- @version October 2016
--- @category web
------------------------------------------------------------------------------

{-# OPTIONS_CYMAKE -Wno-incomplete-patterns #-}

module HTML.Base
 ( HtmlExp(..), textOf,
   HtmlPage(..), PageParam(..),
   HtmlForm(..), FormParam(..), CookieParam(..),
   CgiRef,idOfCgiRef,CgiEnv,HtmlHandler,
   defaultEncoding,
   form,standardForm,answerText,answerEncText,
   cookieForm,getCookies,
   page,standardPage,
   pageEnc,pageCSS,pageMetaInfo,pageLinkInfo,pageBodyAttr,addPageParam,
   formEnc,formCSS,formMetaInfo,formBodyAttr,addFormParam,addFormParams,
   htxt,htxts,hempty,nbsp,h1,h2,h3,h4,h5,
   par,section,header,footer,emphasize,strong,bold,italic,nav,code,
   center,blink,teletype,pre,verbatim,address,href,anchor,
   ulist,olist,litem,dlist,table,headedTable,addHeadings,
   hrule,breakline,image,
   styleSheet,style,textstyle,blockstyle,inline,block,
   redirect,expires,
   button,resetbutton,imageButton,coordinates,
   textfield,password,textarea,checkbox,checkedbox,
   radio_main,radio_main_off,radio_other,
   selection,selectionInitial,multipleSelection,
   hiddenfield,htmlQuote,htmlIsoUmlauts,addAttr,addAttrs,addClass,
   showHtmlExps,showHtmlExp,showHtmlPage,
   htmlPrelude, htmlTagAttrs,
   getUrlParameter,urlencoded2string,string2urlencoded,
   addSound,addCookies, formatCookie
 ) where

import Char        ( isAlphaNum, isSpace )
import ReadNumeric ( readNat, readHex )
import System      ( getEnviron )
import Time        ( CalendarTime(..), ClockTime, toTimeString, toUTCTime )

infixl 0 `addAttr`
infixl 0 `addAttrs`
infixl 0 `addClass`
infixl 0 `addPageParam`
infixl 0 `addFormParam`

------------------------------------------------------------------------------
--- The default encoding used in generated web pages.
defaultEncoding :: String
defaultEncoding = "utf-8" --"iso-8859-1"

------------------------------------------------------------------------------
--- The (abstract) data type for representing references to input elements
--- in HTML forms.
data CgiRef = CgiRef String

--- Internal identifier of a CgiRef (intended only for internal use in other
--- libraries!).
idOfCgiRef :: CgiRef -> String
idOfCgiRef (CgiRef i) = i

--- The type for representing cgi environments
--- (i.e., mappings from cgi references to the corresponding values of
--- the input elements).
type CgiEnv = CgiRef -> String

--- The type of event handlers in HTML forms.
type HtmlHandler = CgiEnv -> IO HtmlForm

--- The data type for representing HTML expressions.
--- @cons HtmlText s - a text string without any further structure
--- @cons HtmlStruct t as hs - a structure with a tag, attributes, and
---                            HTML expressions inside the structure
--- @cons HtmlCRef h ref - an input element (described by the first argument)
---                        with a cgi reference
--- @cons HtmlEvent h hdlr - an input element (first arg) with an associated
---                          event handler (tpyically, a submit button)
data HtmlExp =
   HtmlText String
 | HtmlStruct String [(String,String)] [HtmlExp]
 | HtmlCRef   HtmlExp CgiRef
 | HtmlEvent  HtmlExp HtmlHandler

--- Extracts the textual contents of a list of HTML expressions.
---
--- For instance,
--- <code>textOf [HtmlText "xy", HtmlStruct "a" [] [HtmlText "bc"]] == "xy bc"</code>
textOf :: [HtmlExp] -> String
textOf = unwords . filter (not . null) . map textOfHtmlExp
 where
   textOfHtmlExp (HtmlText s) = s
   textOfHtmlExp (HtmlStruct _ _ hs) = textOf hs
   textOfHtmlExp (HtmlCRef   hexp _) = textOf [hexp]
   textOfHtmlExp (HtmlEvent  hexp _) = textOf [hexp]


------------------------------------------------------------------------------
--- The data type for representing HTML forms (active web pages)
--- and return values of HTML forms.
--- @cons HtmlForm t ps hs - an HTML form with title t, optional parameters
---         (e.g., cookies) ps, and contents hs
--- @cons HtmlAnswer t c - an answer in an arbitrary format where t
---         is the content type (e.g., "text/plain") and c is the contents
data HtmlForm =
        HtmlForm String [FormParam] [HtmlExp]
      | HtmlAnswer String String -- content type (e.g., "text/plain") / content

--- The possible parameters of an HTML form.
--- The parameters of a cookie (FormCookie) are its name and value and
--- optional parameters (expiration date, domain, path (e.g., the path "/"
--- makes the cookie valid for all documents on the server), security) which
--- are collected in a list.
--- @cons FormCookie name value params - a cookie to be sent to the
---                                      client's browser
--- @cons FormCSS s - a URL for a CSS file for this form
--- @cons FormJScript s - a URL for a Javascript file for this form
--- @cons FormOnSubmit s - a JavaScript statement to be executed when the form
---                        is submitted (i.e., &lt;form ... onsubmit="s"&gt;)
--- @cons FormTarget s - a name of a target frame where the output of the
---                      script should be represented (should only be used
---                      for scripts running in a frame)
--- @cons FormEnc - the encoding scheme of this form
--- @cons FormMeta as - meta information (in form of attributes) for this form
--- @cons HeadInclude he - HTML expression to be included in form header
--- @cons MultipleHandlers - indicates that the event handlers of the form
---   can be multiply used (i.e., are not deleted if the form is submitted
---   so that they are still available when going back in the browser;
---   but then there is a higher risk that the web server process
---   might overflow with unused events); the default is a single use
---   of event handlers, i.e., one cannot use the back button in the
---   browser and submit the same form again (which is usually
---   a reasonable behavior to avoid double submissions of data).
--- @cons BodyAttr  ps - optional attribute for the body element (more than
---                      one occurrence is allowed)
data FormParam = FormCookie   String String [CookieParam]
               | FormCSS      String
               | FormJScript  String
               | FormOnSubmit String
               | FormTarget   String
               | FormEnc      String
               | FormMeta     [(String,String)]
               | HeadInclude  HtmlExp
               | MultipleHandlers
               | BodyAttr     (String,String)

--- An encoding scheme for a HTML form.
formEnc :: String -> FormParam
formEnc enc = FormEnc enc

--- A URL for a CSS file for a HTML form.
formCSS :: String -> FormParam
formCSS css = FormCSS css

--- Meta information for a HTML form. The argument is a list of
--- attributes included in the `meta`-tag in the header for this form.
formMetaInfo :: [(String,String)] -> FormParam
formMetaInfo attrs = FormMeta attrs

--- Optional attribute for the body element of the HTML form.
--- More than one occurrence is allowed, i.e., all such attributes are
--- collected.
formBodyAttr :: (String,String) -> FormParam
formBodyAttr attr = BodyAttr attr

--- A cookie to be sent to the client's browser when a HTML form is
--- requested.
formCookie :: (String,String) -> FormParam
formCookie (n,v) = FormCookie n v []

--- The possible parameters of a cookie.
data CookieParam = CookieExpire ClockTime
                 | CookieDomain String
                 | CookiePath   String
                 | CookieSecure

--- A basic HTML form for active web pages with the default encoding
--- and a default background.
--- @param title - the title of the form
--- @param hexps - the form's body (list of HTML expressions)
--- @return an HTML form
form :: String -> [HtmlExp] -> HtmlForm
form title hexps = HtmlForm title [] hexps

--- A standard HTML form for active web pages where the title is included
--- in the body as the first header.
--- @param title - the title of the form
--- @param hexps - the form's body (list of HTML expressions)
--- @return an HTML form with the title as the first header
standardForm :: String -> [HtmlExp] -> HtmlForm
standardForm title hexps = form title (h1 [htxt title] : hexps)

--- An HTML form with simple cookies.
--- The cookies are sent to the client's browser together with this form.
--- @param title - the title of the form
--- @param cookies - the cookies as a list of name/value pairs
--- @param hexps - the form's body (list of HTML expressions)
--- @return an HTML form
cookieForm :: String -> [(String,String)] -> [HtmlExp] -> HtmlForm
cookieForm t cs he = HtmlForm t (map (\(n,v)->FormCookie n v []) cs) he

--- Add simple cookie to HTML form.
--- The cookies are sent to the client's browser together with this form.
--- @param cs - the cookies as a list of name/value pairs
--- @param form - the form to add cookies to
--- @return a new HTML form
addCookies :: [(String,String)] -> HtmlForm -> HtmlForm
addCookies cs (HtmlForm t fas hs) =
  HtmlForm t (map (\ (n,v) -> FormCookie n v []) cs ++ fas) hs
addCookies _ (HtmlAnswer _ _) =
  error "addCookies: cannot add cookie to Html answer"

-- Shows the cookie in standard syntax:
formatCookie :: (String,String,[CookieParam]) -> String
formatCookie (name,value,params) =
  "Set-Cookie: " ++ name ++ "=" ++ string2urlencoded value ++
  concatMap (\p->"; "++formatCookieParam p) params

-- Formats a cookie parameter:
formatCookieParam :: CookieParam -> String
formatCookieParam (CookieExpire e) = "expires=" ++ toCookieDateString e
formatCookieParam (CookieDomain d) = "domain="  ++ d
formatCookieParam (CookiePath   p) = "path="    ++ p
formatCookieParam CookieSecure     = "secure"

-- Formats a clock time into a date string for cookies:
toCookieDateString :: ClockTime -> String
toCookieDateString time =
 let (CalendarTime y mo d h mi s tz) = toUTCTime time
  in (show d ++ "-" ++ shortMonths!!(mo-1) ++ "-" ++ show y ++ " " ++
         toTimeString (CalendarTime y mo d h mi s tz) ++ " UTC")
  where shortMonths = ["Jan","Feb","Mar","Apr","May","Jun",
                       "Jul","Aug","Sep","Oct","Nov","Dec"]


--- A textual result instead of an HTML form as a result for active web pages.
--- @param txt - the contents of the result page
--- @return an HTML answer form
answerText :: String -> HtmlForm
answerText = HtmlAnswer "text/plain"

--- A textual result instead of an HTML form as a result for active web pages
--- where the encoding is given as the first parameter.
--- @param enc - the encoding of the text(e.g., "utf-8" or "iso-8859-1")
--- @param txt - the contents of the result page
--- @return an HTML answer form
answerEncText :: String -> String -> HtmlForm
answerEncText enc = HtmlAnswer ("text/plain; charset="++enc)

--- Adds a parameter to an HTML form.
--- @param form - a form
--- @param param - a form's parameter
--- @return an HTML form
addFormParam :: HtmlForm -> FormParam -> HtmlForm
addFormParam (HtmlForm title params hexps) param =
              HtmlForm title (param:params) hexps
addFormParam hexp@(HtmlAnswer _ _) _ = hexp

addFormParams :: HtmlForm -> [FormParam] -> HtmlForm
addFormParams hform [] = hform
addFormParams hform (fp:fps) = addFormParams (hform `addFormParam` fp) fps

--- Adds redirection to given HTML form.
--- @param secs - Number of seconds to wait before executing autromatic redirection
--- @param url - The URL whereto redirect to
--- @param form - The form to add the header information to
redirect :: Int -> String -> HtmlForm -> HtmlForm
redirect secs url hform =
  hform `addFormParam`
      HeadInclude (HtmlStruct "meta" [("http-equiv","refresh"),
                                      ("content",show secs++"; URL="++url)] [])

--- Adds expire time to given HTML form.
--- @param secs - Number of seconds before document expires
--- @param form - The form to add the header information to
expires :: Int -> HtmlForm -> HtmlForm
expires secs hform =
  hform `addFormParam`
      HeadInclude (HtmlStruct "meta" [("http-equiv","expires"),
                                      ("content",show secs)] [])

--- Adds sound to given HTML form. The functions adds two different declarations
--- for sound, one invented by Microsoft for the internet explorer, one introduced
--- for netscape. As neither is an official part of HTML, addsound might not work
--- on all systems and browsers. The greatest chance is by using sound files
--- in MID-format.
--- @param soundfile - Name of file containing the sound to be played
--- @param loop - Should sound go on infinitely? Use with care.
--- @param form - The form to add sound to
addSound :: String -> Bool -> HtmlForm -> HtmlForm
addSound soundfile loop (HtmlForm title params conts) =
  HtmlForm title
           (HeadInclude
             (HtmlStruct "bgsound"
                         [("src",soundfile),
                         ("loop",if loop then "infinite" else "1")] []):params)
           (HtmlStruct "embed"
             ((if loop then [("loop","true")] else []) ++
              [("src",soundfile),("autostart","true"), ("hidden","true"),
               ("height","0"), ("width","0")]) []:
              conts)
addSound _ _ (HtmlAnswer _ _)
  = error "HTML.addSound: unable to add sound to general HTML Answer"


------------------------------------------------------------------------------
--- The data type for representing HTML pages.
--- The constructor arguments are the title, the parameters, and
--- the contents (body) of the web page.
data HtmlPage = HtmlPage String [PageParam] [HtmlExp]

--- The possible parameters of an HTML page.
--- @cons PageEnc - the encoding scheme of this page
--- @cons PageCSS s - a URL for a CSS file for this page
--- @cons PageJScript s - a URL for a Javascript file for this page
--- @cons PageMeta as - meta information (in form of attributes) for this page
--- @cons PageLink as - link information (in form of attributes) for this page
--- @cons PageBodyAttr attr - optional attribute for the body element of the
---                           page (more than one occurrence is allowed)
data PageParam = PageEnc      String
               | PageCSS      String
               | PageJScript  String
               | PageMeta     [(String,String)]
               | PageLink     [(String,String)]
               | PageBodyAttr (String,String)

--- An encoding scheme for a HTML page.
pageEnc :: String -> PageParam
pageEnc enc = PageEnc enc

--- A URL for a CSS file for a HTML page.
pageCSS :: String -> PageParam
pageCSS css = PageCSS css

--- Meta information for a HTML page. The argument is a list of
--- attributes included in the `meta`-tag in the header for this page.
pageMetaInfo :: [(String,String)] -> PageParam
pageMetaInfo attrs = PageMeta attrs

--- Link information for a HTML page. The argument is a list of
--- attributes included in the `link`-tag in the header for this page.
pageLinkInfo :: [(String,String)] -> PageParam
pageLinkInfo attrs = PageLink attrs

--- Optional attribute for the body element of the web page.
--- More than one occurrence is allowed, i.e., all such attributes are
--- collected.
pageBodyAttr :: (String,String) -> PageParam
pageBodyAttr attr = PageBodyAttr attr

--- A basic HTML web page with the default encoding.
--- @param title - the title of the page
--- @param hexps - the page's body (list of HTML expressions)
--- @return an HTML page
page :: String -> [HtmlExp] -> HtmlPage
page title hexps = HtmlPage title [PageEnc defaultEncoding] hexps

--- A standard HTML web page where the title is included
--- in the body as the first header.
--- @param title - the title of the page
--- @param hexps - the page's body (list of HTML expressions)
--- @return an HTML page with the title as the first header
standardPage :: String -> [HtmlExp] -> HtmlPage
standardPage title hexps = page title (h1 [htxt title] : hexps)

--- Adds a parameter to an HTML page.
--- @param form - a page
--- @param param - a page's parameter
--- @return an HTML page
addPageParam :: HtmlPage -> PageParam -> HtmlPage
addPageParam (HtmlPage title params hexps) param =
  HtmlPage title (param:params) hexps


------------------------------------------------------------------------------
-- some useful abbreviations:

--- Basic text as HTML expression.
--- The text may contain special HTML chars (like &lt;,&gt;,&amp;,&quot;)
--- which will be quoted so that they appear as in the parameter string.
htxt   :: String -> HtmlExp
htxt s = HtmlText (htmlQuote s)

--- A list of strings represented as a list of HTML expressions.
--- The strings may contain special HTML chars that will be quoted.
htxts :: [String] -> [HtmlExp]
htxts = map htxt

--- An empty HTML expression.
hempty :: HtmlExp
hempty = HtmlText ""

--- Non breaking Space
nbsp   :: HtmlExp
nbsp = HtmlText "&nbsp;"

--- Header 1
h1      :: [HtmlExp] -> HtmlExp
h1 hexps = HtmlStruct "h1" [] hexps

--- Header 2
h2      :: [HtmlExp] -> HtmlExp
h2 hexps = HtmlStruct "h2" [] hexps

--- Header 3
h3      :: [HtmlExp] -> HtmlExp
h3 hexps = HtmlStruct "h3" [] hexps

--- Header 4
h4      :: [HtmlExp] -> HtmlExp
h4 hexps = HtmlStruct "h4" [] hexps

--- Header 5
h5      :: [HtmlExp] -> HtmlExp
h5 hexps = HtmlStruct "h5" [] hexps

--- Paragraph
par      :: [HtmlExp] -> HtmlExp
par hexps = HtmlStruct "p" [] hexps

--- Section
section :: [HtmlExp] -> HtmlExp
section hexps = HtmlStruct "section" [] hexps

--- Header
header :: [HtmlExp] -> HtmlExp
header hexps = HtmlStruct "header" [] hexps

--- Footer
footer :: [HtmlExp] -> HtmlExp
footer hexps = HtmlStruct "footer" [] hexps

--- Emphasize
emphasize      :: [HtmlExp] -> HtmlExp
emphasize hexps = HtmlStruct "em" [] hexps

--- Strong (more emphasized) text.
strong      :: [HtmlExp] -> HtmlExp
strong hexps = HtmlStruct "strong" [] hexps

--- Boldface
bold      :: [HtmlExp] -> HtmlExp
bold hexps = HtmlStruct "b" [] hexps

--- Italic
italic      :: [HtmlExp] -> HtmlExp
italic hexps = HtmlStruct "i" [] hexps

--- Navigation
nav :: [HtmlExp] -> HtmlExp
nav doc = HtmlStruct "nav" [] doc

--- Program code
code      :: [HtmlExp] -> HtmlExp
code hexps = HtmlStruct "code" [] hexps

--- Centered text
center      :: [HtmlExp] -> HtmlExp
center hexps = HtmlStruct "center" [] hexps

--- Blinking text
blink      :: [HtmlExp] -> HtmlExp
blink hexps = HtmlStruct "blink" [] hexps

--- Teletype font
teletype      :: [HtmlExp] -> HtmlExp
teletype hexps = HtmlStruct "tt" [] hexps

--- Unformatted input, i.e., keep spaces and line breaks and
--- don't quote special characters.
pre      :: [HtmlExp] -> HtmlExp
pre hexps = HtmlStruct "pre" [] hexps

--- Verbatim (unformatted), special characters (&lt;,&gt;,&amp;,&quot;)
--- are quoted.
verbatim  :: String -> HtmlExp
verbatim s = HtmlStruct "pre" [] [HtmlText (htmlQuote s)]

--- Address
address       :: [HtmlExp] -> HtmlExp
address hexps = HtmlStruct "address" [] hexps

--- Hypertext reference
href           :: String -> [HtmlExp] -> HtmlExp
href ref hexps = HtmlStruct "a" [("href",ref)] hexps

--- An anchored text with a hypertext reference inside a document.
anchor           :: String -> [HtmlExp] -> HtmlExp
anchor anc hexps = HtmlStruct "span" [("id",anc)] hexps

--- Unordered list
--- @param items - the list items where each item is a list of HTML expressions
ulist       :: [[HtmlExp]] -> HtmlExp
ulist items = HtmlStruct "ul" [] (map litem items)

--- Ordered list
--- @param items - the list items where each item is a list of HTML expressions
olist :: [[HtmlExp]] -> HtmlExp
olist items = HtmlStruct "ol" [] (map litem items)

--- A single list item (usually not explicitly used)
litem :: [HtmlExp] -> HtmlExp
litem hexps = HtmlStruct "li" [] hexps

--- Description list
--- @param items - a list of (title/description) pairs (of HTML expressions)
dlist       :: [([HtmlExp],[HtmlExp])] -> HtmlExp
dlist items = HtmlStruct "dl" [] (concatMap ditem items)
 where
   ditem (hexps1,hexps2) = [HtmlStruct "dt" [] hexps1,
                            HtmlStruct "dd" [] hexps2]

--- Table with a matrix of items where each item is a list of HTML expressions.
table :: [[[HtmlExp]]] -> HtmlExp
table items = HtmlStruct "table" []
 (map (\row->HtmlStruct "tr" []
                 (map (\item -> HtmlStruct "td" [] item) row)) items)

--- Similar to <code>table</code> but introduces header tags for the first row.
headedTable :: [[[HtmlExp]]] -> HtmlExp
headedTable = withinTable . table
 where
  withinTable (HtmlStruct "table" attrs (HtmlStruct "tr" rowAttrs row:rows)) =
      HtmlStruct "table" attrs
        (HtmlStruct "tr" rowAttrs (map addTh row):rows)
  addTh x = case x of
             (HtmlStruct "td" attrs conts) -> HtmlStruct "th" attrs conts
             other -> other

--- Add a row of items (where each item is a list of HTML expressions)
--- as headings to a table. If the first argument is not a table,
--- the headings are ignored.
addHeadings :: HtmlExp -> [[HtmlExp]] -> HtmlExp
addHeadings htable headings = case htable of
   HtmlStruct "table" attrs rows ->
      HtmlStruct "table" attrs
         (HtmlStruct "tr" [] (map (\item->HtmlStruct "th" [] item) headings):rows)
   _ -> htable


--- Horizontal rule
hrule :: HtmlExp
hrule = HtmlStruct "hr" [] []

--- Break a line
breakline :: HtmlExp
breakline = HtmlStruct "br" [] []

--- Image
--- @param src - the URL of the image
--- @param alt - the alternative text shown instead of the image
image :: String -> String -> HtmlExp
image src alt = HtmlStruct "img" [("src",src),("alt",htmlQuote alt)] []


-------------- styles and document structuring:
--- Defines a style sheet to be used in this HTML document.
--- @param css - a string in CSS format
styleSheet :: String -> HtmlExp
styleSheet css = HtmlStruct "style" [("type","text/css")] [HtmlText css]

--- Provides a style for HTML elements.
--- The style argument is the name of a style class defined in a
--- style definition (see <code>styleSheet</code>) or in an
--- external style sheet (see form and page parameters <code>FormCSS</code>
--- and <code>PageCSS</code>).
--- @param st - name of a style class
--- @param hexps - list of HTML expressions
style :: String -> [HtmlExp] -> HtmlExp
style st hexps = HtmlStruct "span" [("class",st)] hexps

--- Provides a style for a basic text.
--- The style argument is the name of a style class defined in an
--- external style sheet.
--- @param st - name of a style class
--- @param txt - a string (special characters will be quoted)
textstyle :: String -> String -> HtmlExp
textstyle st txt = HtmlStruct "span" [("class",st)] [htxt txt]

--- Provides a style for a block of HTML elements.
--- The style argument is the name of a style class defined in an
--- external style sheet. This element is used (in contrast to "style")
--- for larger blocks of HTML elements since a line break is placed
--- before and after these elements.
--- @param st - name of a style class
--- @param hexps - list of HTML expressions
blockstyle :: String -> [HtmlExp] -> HtmlExp
blockstyle st hexps = HtmlStruct "div" [("class",st)] hexps

--- Joins a list of HTML elements into a single HTML element.
--- Although this construction has no rendering, it is sometimes useful
--- for programming when several HTML elements must be put together.
--- @param hexps - list of HTML expressions
inline :: [HtmlExp] -> HtmlExp
inline hexps = HtmlStruct "span" [] hexps

--- Joins a list of HTML elements into a block.
--- A line break is placed before and after these elements.
--- @param hexps - list of HTML expressions
block :: [HtmlExp] -> HtmlExp
block hexps = HtmlStruct "div" [] hexps


-------------- forms and input fields:
--- Submit button with a label string and an event handler
button :: String -> HtmlHandler -> HtmlExp
button label handler =
    HtmlEvent
       (HtmlStruct "input" [("type","submit"),("name","EVENT"),
                            ("value",htmlQuote label)] [])
       handler

--- Reset button with a label string
resetbutton :: String -> HtmlExp
resetbutton label =
    HtmlStruct "input" [("type","reset"),("value",htmlQuote label)] []

--- Submit button in form of an imag.
--- @param src - url of the image
--- @param handler - event handler
imageButton :: String -> HtmlHandler -> HtmlExp
imageButton src handler
  = HtmlEvent
       (HtmlStruct "input" [("type","image"),("name","EVENT"),("src",src)] [])
       handler

--- Input text field with a reference and an initial contents
textfield :: CgiRef -> String -> HtmlExp
textfield cref contents
  | cref =:= CgiRef ref -- instantiate cref argument
  = HtmlCRef
       (HtmlStruct "input" [("type","text"),("name",ref),
                            ("value",htmlQuote contents)] [])
       cref
 where ref free

--- Input text field (where the entered text is obscured) with a reference
password :: CgiRef -> HtmlExp
password cref
  | cref =:= CgiRef ref -- instantiate cref argument
  = HtmlCRef
       (HtmlStruct "input" [("type","password"),("name",ref)] [])
       cref
 where
   ref free

--- Input text area with a reference, height/width, and initial contents
textarea :: CgiRef -> (Int,Int) -> String -> HtmlExp
textarea cref (height,width) contents
  | cref =:= CgiRef ref -- instantiate cref argument
  = HtmlCRef
       (HtmlStruct "textarea" [("name",ref),
                                ("rows",show height),("cols",show width)]
                               [htxt contents])
       cref
 where
   ref free

--- A checkbox with a reference and a value.
--- The value is returned if checkbox is on, otherwise "" is returned.
checkbox :: CgiRef -> String -> HtmlExp
checkbox cref value
  | cref =:= CgiRef ref -- instantiate cref argument
  = HtmlCRef
       (HtmlStruct "input" [("type","checkbox"),("name",ref),
                            ("value",htmlQuote value)] [])
       cref
 where
   ref free

--- A checkbox that is initially checked with a reference and a value.
--- The value is returned if checkbox is on, otherwise "" is returned.
checkedbox :: CgiRef -> String -> HtmlExp
checkedbox cref value
  | cref =:= CgiRef ref -- instantiate cref argument
  = HtmlCRef
       (HtmlStruct "input" [("type","checkbox"),("name",ref),
                            ("value",htmlQuote value),("checked","checked")] [])
       cref
 where
   ref free

--- A main button of a radio (initially "on") with a reference and a value.
--- The value is returned of this button is on.
--- A complete radio button suite always consists of a main button
--- (radio_main) and some further buttons (radio_others) with the
--- same reference. Initially, the main button is selected
--- (or nothing is selected if one uses radio_main_off instead of radio_main).
--- The user can select another button but always at most one button
--- of the radio can be selected. The value corresponding to the
--- selected button is returned in the environment for this radio reference.
radio_main :: CgiRef -> String -> HtmlExp
radio_main cref value
  | cref =:= CgiRef ref -- instantiate cref argument
  = HtmlCRef
       (HtmlStruct "input" [("type","radio"),("name",ref),
                            ("value",htmlQuote value),("checked","yes")] [])
       cref
 where
   ref free

--- A main button of a radio (initially "off") with a reference and a value.
--- The value is returned of this button is on.
radio_main_off :: CgiRef -> String -> HtmlExp
radio_main_off cref value
  | cref =:= CgiRef ref -- instantiate cref argument
  = HtmlCRef
       (HtmlStruct "input" [("type","radio"),("name",ref),
                            ("value",htmlQuote value)] [])
       cref
 where
   ref free

--- A further button of a radio (initially "off") with a reference (identical
--- to the main button of this radio) and a value.
--- The value is returned of this button is on.
radio_other :: CgiRef -> String -> HtmlExp
radio_other cref value
  | cref =:= CgiRef ref -- instantiate cref argument
  = HtmlStruct "input"
               [("type","radio"),("name",ref),("value",htmlQuote value)] []
 where
   ref free

--- A selection button with a reference and a list of name/value pairs.
--- The names are shown in the selection and the value is returned
--- for the selected name.
selection :: CgiRef -> [(String,String)] -> HtmlExp
selection cref menue
  | cref =:= CgiRef ref -- instantiate cref argument
  = HtmlCRef
       (HtmlStruct "select" [("name",ref)]
         ((concat . map (\(n,v)->[HtmlStruct "option" [("value",v)] [htxt n]]))
          menue))
       cref
 where
   ref free

--- A selection button with a reference, a list of name/value pairs,
--- and a preselected item in this list.
--- The names are shown in the selection and the value is returned
--- for the selected name.
--- @param ref - a CGI reference
--- @param nvs - list of name/value pairs
--- @param sel - the index of the initially selected item in the list nvs
--- @return an HTML expression representing the selection button
selectionInitial :: CgiRef -> [(String,String)] -> Int -> HtmlExp
selectionInitial cref sellist sel
  | cref =:= CgiRef ref -- instantiate cref argument
  = HtmlCRef (HtmlStruct "select" [("name",ref)] (selOption sellist sel))
             cref
 where
   ref free

   selOption [] _ = []
   selOption ((n,v):nvs) i =
      HtmlStruct "option"
        ([("value",v)] ++ if i==0 then [("selected","selected")] else [])
        [htxt n] : selOption nvs (i-1)

--- A selection button with a reference and a list of name/value/flag pairs.
--- The names are shown in the selection and the value is returned
--- if the corresponding name is selected. If flag is True, the
--- corresonding name is initially selected. If more than one name
--- has been selected, all values are returned in one string
--- where the values are separated by newline (`'\n'`) characters.
multipleSelection :: CgiRef -> [(String,String,Bool)] -> HtmlExp
multipleSelection cref sellist
  | cref =:= CgiRef ref -- instantiate cref argument
  = HtmlCRef (HtmlStruct "select" [("name",ref),("multiple","multiple")]
                                  (map selOption sellist))
            cref
 where
   ref free

   selOption (n,v,flag) =
      HtmlStruct "option"
        ([("value",v)] ++ if flag then [("selected","selected")] else [])
        [htxt n]

--- A hidden field to pass a value referenced by a fixed name.
--- This function should be used with care since it may cause
--- conflicts with the CGI-based implementation of this library.
hiddenfield :: String -> String -> HtmlExp
hiddenfield name value =
    HtmlStruct "input" [("type","hidden"),("name",name),("value",value)] []


------------------------------------------------------------------------------
--- Quotes special characters (`<`,`>`,`&`,`"`, umlauts) in a string
--- as HTML special characters.
htmlQuote :: String -> String
htmlQuote [] = []
htmlQuote (c:cs) | c=='<' = "&lt;"   ++ htmlQuote cs
                 | c=='>' = "&gt;"   ++ htmlQuote cs
                 | c=='&' = "&amp;"  ++ htmlQuote cs
                 | c=='"' = "&quot;" ++ htmlQuote cs
                 | otherwise = htmlIsoUmlauts [c] ++ htmlQuote cs

--- Translates umlauts in iso-8859-1 encoding into HTML special characters.
htmlIsoUmlauts :: String -> String
htmlIsoUmlauts [] = []
htmlIsoUmlauts (c:cs) | oc==228 = "&auml;"  ++ htmlIsoUmlauts cs
                      | oc==246 = "&ouml;"  ++ htmlIsoUmlauts cs
                      | oc==252 = "&uuml;"  ++ htmlIsoUmlauts cs
                      | oc==196 = "&Auml;"  ++ htmlIsoUmlauts cs
                      | oc==214 = "&Ouml;"  ++ htmlIsoUmlauts cs
                      | oc==220 = "&Uuml;"  ++ htmlIsoUmlauts cs
                      | oc==223 = "&szlig;" ++ htmlIsoUmlauts cs
                      | oc==197 = "&Aring;" ++ htmlIsoUmlauts cs
                      | oc==250 = "&uacute;"++ htmlIsoUmlauts cs
                      | oc==237 = "&iacute;"++ htmlIsoUmlauts cs
                      | oc==225 = "&aacute;"++ htmlIsoUmlauts cs
                      | otherwise = c : htmlIsoUmlauts cs
  where oc = ord c

------------------------------------------------------------------------------
--- Adds an attribute (name/value pair) to an HTML element.
addAttr :: HtmlExp -> (String,String) -> HtmlExp
addAttr hexp attr = addAttrs hexp [attr]

--- Adds a list of attributes (name/value pair) to an HTML element.
addAttrs :: HtmlExp -> [(String,String)] -> HtmlExp
addAttrs (HtmlText s) _ = HtmlText s  -- strings have no attributes
addAttrs (HtmlStruct tag attrs hexps) newattrs =
    HtmlStruct tag (attrs++newattrs) hexps
addAttrs (HtmlEvent hexp handler) attrs =
    HtmlEvent (addAttrs hexp attrs) handler
addAttrs (HtmlCRef  hexp cref) attrs =
    HtmlCRef (addAttrs hexp attrs) cref

--- Adds a class attribute to an HTML element.
addClass :: HtmlExp -> String -> HtmlExp
addClass hexp cls = addAttr hexp ("class",cls)

------------------------------------------------------------------------------
-- Auxiliaries for faster show (could be later put into a standard library)

type ShowS = String -> String

showString :: String -> String -> String
showString s = (s++)

showChar :: Char -> String -> String
showChar c = (c:)

nl :: String -> String
nl = showChar '\n'

concatS :: [a -> a] -> a -> a
concatS [] = id
concatS xs@(_:_) = foldr1 (\ f g -> f . g) xs

------------------------------------------------------------------------------
--- Transforms a list of HTML expressions into string representation.
showHtmlExps :: [HtmlExp] -> String
showHtmlExps hexps = showsHtmlExps 0 hexps ""

-- get the string contents of an HTML expression:
getText :: HtmlExp -> String
getText (HtmlText s)       = s
getText (HtmlStruct _ _ _) = ""
getText (HtmlEvent  he _)  = getText he
getText (HtmlCRef   he _)  = getText he

-- get the (last) tag of an HTML expression:
getTag :: HtmlExp -> String
getTag (HtmlText _)         = ""
getTag (HtmlStruct tag _ _) = tag
getTag (HtmlEvent  he _)    = getTag he
getTag (HtmlCRef   he _)    = getTag he

-- is this a tag where a line break can be safely added?
tagWithLn :: String -> Bool
tagWithLn t = t/="" &&
              t `elem` ["br","p","li","ul","ol","dl","dt","dd","hr",
                        "h1","h2","h3","h4","h5","h6","div",
                        "html","title","head","body","link","meta","script",
                        "form","table","tr","td"]


--- Transforms a single HTML expression into string representation.
showHtmlExp :: HtmlExp -> String
showHtmlExp hexp = showsHtmlExp 0 hexp ""

--- HTML tags that have no end tag in HTML:
noEndTags :: [String]
noEndTags = ["img","input","link","meta"]

showsHtmlExp :: Int -> HtmlExp -> ShowS
showsHtmlExp _ (HtmlText s) = showString s
showsHtmlExp i (HtmlStruct tag attrs hexps) =
  let maybeLn j = if tagWithLn tag then nl . showTab j else id
   in maybeLn i .
      (if null hexps && (null attrs || tag `elem` noEndTags)
       then showsHtmlOpenTag tag attrs "/>"
       else showsHtmlOpenTag tag attrs ">" . maybeLn (i+2) . showExps hexps .
            maybeLn i . showString "</" . showString tag . showChar '>'
      ) . maybeLn i
 where
  showExps = if tag=="pre"
             then concatS . map (showsHtmlExp 0) else showsHtmlExps (i+2)
showsHtmlExp i (HtmlEvent hexp _) = showsHtmlExp i hexp
showsHtmlExp i (HtmlCRef  hexp _) = showsHtmlExp i hexp

showsHtmlExps :: Int -> [HtmlExp] -> ShowS
showsHtmlExps _ [] = id
showsHtmlExps i (he:hes) = showsWithLnPrefix he . showsHtmlExps i hes
 where
   showsWithLnPrefix hexp = let s = getText hexp
                            in if s/="" && isSpace (head s)
                               then nl . showTab i . showString (tail s)
                               else showsHtmlExp i hexp

showTab :: Int -> String -> String
showTab n = showString (take n (repeat ' '))

showsHtmlOpenTag :: String -> [(String,String)] -> String -> ShowS
showsHtmlOpenTag tag attrs close =
  showChar '<' . showString tag .
  concatS (map attr2string attrs) . showString close
 where
    attr2string (attr,value) = showChar ' ' . showString attr .
         showString "=\"" . encodeQuotes value . showChar '"'

    -- encode double quotes as "&quot;":
    encodeQuotes [] = id
    encodeQuotes (c:cs) | c=='"'    = showString "&quot;" . encodeQuotes cs
                        | otherwise = showChar c . encodeQuotes cs


------------------------------------------------------------------------------
--- Transforms HTML page into string representation.
--- @param page - the HTML page
--- @return string representation of the HTML document
showHtmlPage :: HtmlPage -> String
showHtmlPage (HtmlPage title params html) =
  htmlPrelude ++
  showHtmlExp (HtmlStruct "html" htmlTagAttrs
                  [HtmlStruct "head" []
                       ([HtmlStruct "title" [] [HtmlText (htmlQuote title)]] ++
                       concatMap param2html params),
                   HtmlStruct "body" bodyattrs html])
 where
  param2html (PageEnc enc) =
     [HtmlStruct "meta" [("http-equiv","Content-Type"),
                         ("content","text/html; charset="++enc)] []]
  param2html (PageCSS css) =
     [HtmlStruct "link" [("rel","stylesheet"),("type","text/css"),("href",css)]
                 []]
  param2html (PageJScript js) =
     [HtmlStruct "script" [("type","text/javascript"),("src",js)] []]
  param2html (PageMeta attrs) = [HtmlStruct "meta" attrs []]
  param2html (PageLink attrs) = [HtmlStruct "link" attrs []]
  param2html (PageBodyAttr _) = [] -- these attributes are separately processed

  bodyattrs = [attr | (PageBodyAttr attr) <- params]

--- Standard header for generated HTML pages.
htmlPrelude :: String
htmlPrelude = "<!DOCTYPE html>\n"

--- Standard attributes for element "html".
htmlTagAttrs :: [(String,String)]
htmlTagAttrs = [("lang","en")]

------------------------------------------------------------------------------
--- Gets the parameter attached to the URL of the script.
--- For instance, if the script is called with URL
--- "http://.../script.cgi?parameter", then "parameter" is
--- returned by this I/O action.
--- Note that an URL parameter should be "URL encoded" to avoid
--- the appearance of characters with a special meaning.
--- Use the functions "urlencoded2string" and "string2urlencoded"
--- to decode and encode such parameters, respectively.

getUrlParameter :: IO String
getUrlParameter = getEnviron "QUERY_STRING"

--- Translates urlencoded string into equivalent ASCII string.
urlencoded2string :: String -> String
urlencoded2string [] = []
urlencoded2string (c:cs)
  | c == '+'  = ' ' : urlencoded2string cs
  | c == '%'  = chr (maybe 0 fst (readHex (take 2 cs)))
                 : urlencoded2string (drop 2 cs)
  | otherwise = c : urlencoded2string cs

--- Translates arbitrary strings into equivalent urlencoded string.
string2urlencoded :: String -> String
string2urlencoded [] = []
string2urlencoded (c:cs)
  | isAlphaNum c = c : string2urlencoded cs
  | c == ' '     = '+' : string2urlencoded cs
  | otherwise = let oc = ord c
    in '%' : int2hex(oc `div` 16) : int2hex(oc `mod` 16) : string2urlencoded cs
 where
   int2hex i = if i<10 then chr (ord '0' + i)
                       else chr (ord 'A' + i - 10)


------------------------------------------------------------------------------
--- Gets the cookies sent from the browser for the current CGI script.
--- The cookies are represented in the form of name/value pairs since
--- no other components are important here.
getCookies :: IO [(String,String)]
getCookies =
 do cookiestring <- getEnviron "HTTP_COOKIE"
    return $ parseCookies cookiestring

-- translate a string of cookies (of the form "NAME1=VAL1; NAME2=VAL")
-- into a list of name/value pairs:
parseCookies :: String -> [(String,String)]
parseCookies str = if str=="" then [] else
  let (c1,cs) = break (==';') str
   in parseCookie c1 :
      parseCookies (dropWhile (==' ') (if cs=="" then "" else tail cs))
 where
  parseCookie s = let (name,evalue) = break (=='=') s in
      (name,if evalue=="" then "" else urlencoded2string (tail evalue))

--- For image buttons: retrieve the coordinates where the user clicked
--- within the image.
coordinates :: CgiEnv -> Maybe (Int,Int)
coordinates env = let x = env (CgiRef "x")
                      y = env (CgiRef "y")
                   in if x/="" && y/=""
                        then Just (tryReadNat 0 x, tryReadNat 0 y)
                        else Nothing
 where
  tryReadNat d s = maybe d (\(i,rs)->if null rs then i else d) (readNat s)

------------------------------------------------------------------------------
