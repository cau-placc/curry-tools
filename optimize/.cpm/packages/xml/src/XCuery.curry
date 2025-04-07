------------------------------------------------------------------------------
--- Library for declarative processing XML data.
--- 
--- This libary contains definitions of combinators which can be used
--- to specify queries and transformations on XML data in a high-level
--- declarative manner. Actually, these combinators can be used as
--- functional patterns in operations to query and transform XML data.
---
--- The ideas and usage of this library is described in the paper
---
--- > M. Hanus: Declarative Processing of Semistructured Web Data.
--- > Technical Communications of the 27th International Conference
--- > on Logic Programming (ICLP 2011),
--- > Leibniz International Proceedings in Informatics (LIPIcs),
--- > Vol. 11, pp. 198-208, 2011
--- > [Online](https://doi.org/10.4230/LIPIcs.ICLP.2011.198)
---
--- @author Michael Hanus
--- @version February 2025
------------------------------------------------------------------------------
{-# OPTIONS_FRONTEND -Wno-overlapping #-}

module XCuery where

import XML

------------------------------------------------------------------------------

--- The operation `with` evaluates to all lists containing the
--- elements in the argument in the same order. For instance,
---
---     > with [1,2]
---     1:2:_a
---     1:_a:2:_b
---     1:_a:_b:2:_c
---     ...
---
--- This operation can be used in a pattern to match XML structures
--- having at least the given elements as children, as in
---
---     getNamePhone
---       (xml "entry"
---            (with [xml "name"  [xtxt name],
---                   xml "phone" [xtxt phone]])) = name ++ ": " ++ phone
with :: Data a => [a] -> [a]
with []     = unknown
with (x:xs) = unknown ++ x : with xs

--- The operation `xml'` can be used to match an XML structure with a given
--- tag and children independent of the attributes. For instance,
---
---     getName (xml’ "entry" (with [xml’ "name" [xtxt n]])) = n
---
--- matches an XML element with tag `entry` and some child element with
--- tag `name`, independent of the possible attributes attached to these
--- elements.
xml' :: String -> [XmlExp] -> XmlExp
xml' t xs = XElem t unknown xs

--- The operation `anyorder` returns all permutations of the input list.
--- It can be used in a pattern to match XML structures with the given
--- elements in arbitrary order, as in
---
---     getNamePhone
---       (xml "entry"
---         (with (anyorder [xml "name"  [xtxt name],
---                          xml "phone" [xtxt phone]]))) = name ++ ": " ++ phone
anyorder :: [a] -> [a]
anyorder []     = []
anyorder (x:xs) = insert (anyorder xs)
 where insert []     = [x]
       insert (y:ys) = x:y:ys  ?  y : insert ys


--- The operation `deepXml` evaluates to any XML structure containing
--- an XML structure with the given tag and children at an abritrarily
--- deep position. For instance,
---
---     getNamePhone
---       (deepXml "entry"
---                (with [xml "name"  [xtxt name],
---                       xml "phone" [xtxt phone]])) = name ++ ": " ++ phone
---
--- allows to query an XML structure having a structure with tag `entry`
--- and children structures with tags `name` and `phone`.
deepXml :: String -> [XmlExp] -> XmlExp
deepXml tag elems = xml tag elems
deepXml tag elems = xml' unknown (unknown ++ [deepXml tag elems] ++ unknown)

--- This operation is similar to 'deepXml' but matches the XML structures
--- independent of their attributes.
deepXml' :: String -> [XmlExp] -> XmlExp
deepXml' tag elems = xml' tag elems
deepXml' tag elems = xml' unknown (unknown ++ [deepXml' tag elems] ++ unknown)

--- This operation is similar to 'deepXml' but allows to provide a list
--- of attributes to the matched XML structures.
--- For instance,
---
---     getMaleFirstNames
---       (deepXElem "first" (with [("sex","male")]) [xtxt f]) = f
---
--- matches an XML structure containing an XML structure with tag `first`
--- and some attribute `sex` with value `male`.
deepXElem :: String -> [(String,String)] -> [XmlExp] -> XmlExp
deepXElem tag attrs elems = XElem tag attrs elems
deepXElem tag attrs elems =
  xml' unknown (unknown ++ [deepXElem tag attrs elems] ++ unknown)

--- The predicate `noTagOf` returns `True` if the given tag is not a tag
--- of all argument XML structures.
noTagOf :: String -> [XmlExp] -> Bool
noTagOf tag = all ((/= tag) . tagOf)

--- The operation `withOthers` is similar to 'with' but has a second argument
--- that contains the child nodes that are present but not part of the
--- first argument. One can use this operation to denote the
--- `unmatched` part of an XML structure in order to put conditions on it.
--- For instance, if one wants to get the name and phone number of an entry
--- that has no email address, one can use the following definition:
--- 
---     getNamePhoneWithoutEmail
---       (deepXml "entry"
---          (withOthers [xml "name" [xtxt name], xml "phone" [xtxt phone]]
---                      others))
---       | "email" `noTagOf` others
---       = name ++ ": " ++ phone
withOthers :: Data a => [a] -> [a] -> [a]
withOthers ys zs = withAcc [] ys zs
 where -- Accumulate remaining elements:
   withAcc prevs [] others | others=:=prevs++suffix = suffix
                                             where suffix free
   withAcc prevs (x:xs) others =
      prefix ++ x : withAcc (prevs++prefix) xs others
                                             where prefix free

--- The operation `optXml` can be used in transformations of XML elements
--- to insert a structure with a given tag depending on the presence
--- of this tag in a list of XML strucutres.
--- For this purpose, `optXml t xs ys` evaluates to `xml t xs`
--- if there is no element with tag `t` in `ys`, otherwise the first element
--- of `ys` with tag `t` is returned.
--- 
--- The following definition shows a usage of `optXml`:
--- 
---     transNickPhone
---       (deepXml "entry"
---                (withOthers [xml "name"  [xtxt n],
---                             xml "first" [xtxt f],
---                             xml "phone" phone]
---                            others)) =
---       xml "nickphone" [optXml "nickname" [xtxt (f++n)] others,
---                        xml "phone" phone]
optXml :: String -> [XmlExp] -> [XmlExp] -> XmlExp
optXml tag elems [] = xml tag elems
optXml tag elems (x:xs) =
  if tag == tagOf x then x else optXml tag elems xs

------------------------------------------------------------------------------
