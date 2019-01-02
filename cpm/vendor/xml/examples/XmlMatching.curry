-- Some examples for matching in XML documents based on the `XCuery` library.
--
-- Note that this module requires the installation
-- of the package `setfunctions`!

import Control.SetFunctions

import XML
import XCuery

import Test.Prop

-- Some sample XML documents:
entry1 :: XmlExp
entry1 = xml "entry"
             [xml "name" [xtxt "Hanus"],
              xml "first" [xtxt "Michael"],
              xml "phone" [xtxt "+49-431-8807271"],
              xml "email" [xtxt "mh@informatik.uni-kiel.de"],
              xml "email" [xtxt "hanus@acm.org"]]

entry2 :: XmlExp
entry2 = xml "entry"
             [xml "phone" [xtxt "+1-987-742-9388"],
              xml "name" [xtxt "Smith"],
              xml "first" [xtxt "William"],
              xml "nickname" [xtxt "Bill"]]

contacts :: XmlExp
contacts = xml "contacts" [entry1,entry2]

-- Search for names and their phone numbers:
getNamePhone :: XmlExp -> String
getNamePhone
  (xml "entry"
       (with [xml "name" [xtxt name],
              xml "phone" [xtxt phone]])) = name ++ ": " ++ phone

test1 = getNamePhone entry1 -=- "Hanus: +49-431-8807271"
test2 = failing $ getNamePhone entry2 -- due to wrong order of phone/name

-- Search for names and their phone numbers appearing in any order:
getAnyNamePhone :: XmlExp -> String
getAnyNamePhone
  (xml "entry"
       (with (anyorder [xml "phone" [xtxt phone],
                        xml "name"  [xtxt name]])))
  = name ++ ": " ++ phone

test3 = getAnyNamePhone entry2 -=- "Smith: +1-987-742-9388"

-- Search for some email occurring anywhere (deep) in a document:
getEmail :: XmlExp -> String
getEmail (deepXml "email" [xtxt email]) = email

test4 = getEmail contacts <~> ("hanus@acm.org" ? "mh@informatik.uni-kiel.de")

-- Get all emails:
allEmails :: [String]
allEmails = sortValues ((set1 getEmail) contacts)

test5 = allEmails -=- ["hanus@acm.org","mh@informatik.uni-kiel.de"]


-- Negated patterns: constructive negation using `withOthers` combinator:
-- get name/phone of all persons without email:
getNamePhoneWithoutEmail :: XmlExp -> String
getNamePhoneWithoutEmail
  (deepXml "entry"
           (withOthers (anyorder [xml "name" [xtxt name],
                                  xml "phone" [xtxt phone]])
                       others))
  | "email" `noTagOf` others
  = name ++ ": " ++ phone

noTagOf :: String -> [XmlExp] -> Bool
noTagOf tag xmlexps = all (\xe -> tag /= (tagOf xe)) xmlexps

test6 = getNamePhoneWithoutEmail contacts <~> "Smith: +1-987-742-9388"


--- Transformation of contact data into the form (phone,fullname):
transPhone :: XmlExp -> XmlExp
transPhone
  (deepXml "entry"
           (with (anyorder ([xml "name" [xtxt name],
                             xml "first" [xtxt first],
                             xml "phone" [xtxt phone]]))))
  = xml "phonename" [xml "phone" [xtxt phone],
                     xml "fullname" [xtxt (first++" "++name)]]

phoneTable :: XmlExp
phoneTable = xml "table" (sortValues ((set1 transPhone) contacts))


-- Collect results:
-- Get all names with number of email addresses
getEmails :: XmlExp -> (String,Int)
getEmails (deepXml "entry"
             (withOthers [xml "name" [xtxt name]] others))
 = (name, length (sortValues ((set1 emailOf) others)))

emailOf :: [XmlExp] -> [XmlExp]
emailOf (with [xml "email" email]) = email

test8 = getEmails contacts <~> (("Hanus",2) ? ("Smith",0))
