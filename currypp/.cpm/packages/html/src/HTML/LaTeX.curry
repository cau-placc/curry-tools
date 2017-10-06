------------------------------------------------------------------------------
--- This library contains operation to transform HTML documents
--- into LaTeX string.
---
--- @author Michael Hanus
--- @version October 2017
--- @category web
------------------------------------------------------------------------------

module HTML.LaTeX
 ( showLatexExps, showLatexExp, showLatexDoc, showLatexDocs
 , showLatexDocsWithPackages, showLatexDocWithPackages
 , germanLatexDoc, htmlSpecialChars2tex
 ) where

import List ( intercalate )

import HTML.Base

--- Transforms HTML expressions into LaTeX string representation.

showLatexExps :: [HtmlExp] -> String
showLatexExps hexps = concat (map showLatexExp hexps)

--- Transforms an HTML expression into LaTeX string representation.
showLatexExp :: HtmlExp -> String
showLatexExp (HtmlText s) = "{" ++ specialchars2tex s ++ "}"
showLatexExp (HtmlStruct tag attrs htmlexp)
 | tag=="html" = showLatexExps htmlexp
 | tag=="head" = ""                    -- ignore header
 | tag=="body" = showLatexExps htmlexp
 | tag=="form" = showLatexExps htmlexp
 | tag=="h1"   = "\\section*{" ++ showLatexExps htmlexp ++ "}\n"
 | tag=="h2"   = "\\subsection*{" ++ showLatexExps htmlexp ++ "}\n"
 | tag=="h3"   = "\\subsubsection*{" ++ showLatexExps htmlexp ++ "}\n"
 | tag=="h4"   = "\\paragraph*{" ++ showLatexExps htmlexp ++ "}\n"
 | tag=="h5"   = "\\subparagraph*{" ++ showLatexExps htmlexp ++ "}\n"
 | tag=="p"    = showLatexExps htmlexp ++ "\\par\n"
 | tag=="b"    = "{\\bf " ++ showLatexExps htmlexp ++ "}"
 | tag=="em"   = "\\emph{" ++ showLatexExps htmlexp ++ "}"
 | tag=="i"    = "{\\it " ++ showLatexExps htmlexp ++ "}"
 | tag=="tt"   = "{\\tt " ++ showLatexExps htmlexp ++ "}"
 | tag=="code" = "{\\tt " ++ showLatexExps htmlexp ++ "}"
 | tag=="center" = latexEnvironment "center" (showLatexExps htmlexp)
 | tag=="pre"  = latexEnvironment "verbatim" (textOf htmlexp)
 | tag=="font" = showLatexExps htmlexp  -- ignore font changes
 | tag=="address" = showLatexExps htmlexp
 | tag=="blink"   = showLatexExps htmlexp
 | tag=="sub"  = "$_{\\mbox{" ++ showLatexExps htmlexp ++ "}}$"
 | tag=="sup"  = "$^{\\mbox{" ++ showLatexExps htmlexp ++ "}}$"
 | tag=="a"    = showLatexExps htmlexp ++
                 -- add href attribute as footnote, if present:
                 maybe ""
                       (\url->"\\footnote{\\tt "++specialchars2tex url++"}\n")
                       (findHtmlAttr "href" attrs)
 | tag=="ul"   = latexEnvironment "itemize"  (showLatexExps htmlexp)
 | tag=="ol"   = latexEnvironment "enumerate" (showLatexExps htmlexp)
 | tag=="li"   = "\\item\n" ++ showLatexExps htmlexp ++ "\n"
 | tag=="dl"   = latexEnvironment "description" (showLatexExps htmlexp)
 | tag=="dt"  = "\\item[" ++ showLatexExps htmlexp ++ "]~\\\\\n"
 | tag=="dd"  = showLatexExps htmlexp
 -- tables will be set using the longtable environment,
 -- (The package longtable is added by default to every latex document)
 | tag=="table" = attrLatexEnv "longtable" (latexTabFormat htmlexp)
                                           (showLatexTableContents htmlexp)
 | tag=="tr"   = let cells = map showLatexExp htmlexp
                  in intercalate " & " cells ++ "\\\\\n"
 | tag=="td"   = showLatexExps htmlexp
 | tag=="br"   = "\\par\n"
 | tag=="hr"   = "\\vspace{2ex}\\hrule\n"
 | tag=="img" = "{" ++  maybe "{\\tt<IMAGE>}" specialchars2tex
                             (findHtmlAttr "alt" attrs)
                    ++ "}"
 | tag=="input" && maybe "" id (findHtmlAttr "type" attrs) == "hidden" = ""
 | otherwise   = "{\\tt<"++tag++">}" ++ showLatexExps htmlexp ++
                "{\\tt</"++tag++">}"
showLatexExp (HtmlCRef  _ _) = error "HTML.LaTeX.showLatexExp: HtmlCref"
showLatexExp (HtmlEvent _ _) = error "HTML.LaTeX.showLatexExp: HtmlEvent"

-- create latex environment of name "env" with content "content"
latexEnvironment :: String -> String -> String
latexEnvironment env content = attrLatexEnv env "" content

-- create latex environment of name "env" with content "content"
-- adding the parameters "attr"
attrLatexEnv :: String -> String -> String -> String
attrLatexEnv env attr content
 = "\\begin{"++env++"}"++attr++"\n"
 ++content
 ++"\n\\end{"++env++"}\n"

-- yield the format of a table, e.g. {lll} from list of html rows.
-- for longtables we set the chunksize big enough
-- to avoid having to rerun latex for inaccurat tables.
latexTabFormat :: [HtmlExp] -> String
latexTabFormat rows = "{" ++ replicate breadth 'l' ++ "}"
  ++ "\\setcounter{LTchunksize}{"++show (length rows+5)++"}%"
  where
    breadth = foldl max 0 (map getBreadth rows)

-- retrieve the breadth of an Html row
getBreadth :: HtmlExp -> Int
getBreadth row = case row of
                     HtmlStruct "tr" _ tds -> length tds
                     _ -> error "getBreadth: no row given"

-- tranlate expressions inside tables
showLatexTableContents :: [HtmlExp] -> String
showLatexTableContents hexps = concatMap showLatexTableContent hexps

-- tranlate expressions inside tables
showLatexTableContent :: HtmlExp -> String
showLatexTableContent (HtmlText s) = "{" ++ specialchars2tex s ++ "}"
showLatexTableContent (HtmlStruct tag attrs htmlexp)
 | tag=="html" = showLatexTableContents htmlexp
 | tag=="head" = ""                    -- ignore header
 | tag=="body" = showLatexTableContents htmlexp
 | tag=="form" = showLatexTableContents htmlexp
 | tag=="p"    = showLatexTableContents htmlexp ++ "\\par\n"
 | tag=="b"    = "{\\bf " ++ showLatexTableContents htmlexp ++ "}"
 | tag=="em"   = "\\emph{" ++ showLatexTableContents htmlexp ++ "}"
 | tag=="i"    = "{\\it " ++ showLatexTableContents htmlexp ++ "}"
 | tag=="tt"   = "{\\tt " ++ showLatexTableContents htmlexp ++ "}"
 | tag=="font" = showLatexTableContents htmlexp  -- ignore font changes
 | tag=="address" = showLatexTableContents htmlexp
 | tag=="blink"   = showLatexTableContents htmlexp
 | tag=="a"    = showLatexTableContents htmlexp ++
                 -- add href attribute as footnote, if present:
                 maybe ""
                       (\url->"\\footnote{\\tt "++specialchars2tex url++"}\n")
                       (findHtmlAttr "href" attrs)
 | tag=="tr"   = let cells = map showLatexTableContent htmlexp
                  in intercalate " & " cells ++ "\\\\\n"
 | tag=="td"   = showLatexTableContents htmlexp
 | tag=="br"   = "\\par\n"
 | tag=="hr"   = "\\vspace{2ex}\\hrule\n"
 | tag=="img"  = "{" ++  maybe "{\\tt<IMAGE>}" specialchars2tex
                               (findHtmlAttr "alt" attrs)
                    ++ "}"
 | tag=="input" && maybe "" id (findHtmlAttr "type" attrs) == "hidden" = ""
 | otherwise   = "{\\tt<"++tag++">}" ++ showLatexTableContents htmlexp ++
                 "{\\tt</"++tag++">}"
showLatexTableContent (HtmlCRef  _ _) =
  error "HTML.LaTeX.showLatexTableContent: HtmlCref"
showLatexTableContent (HtmlEvent _ _) =
  error "HTML.LaTeX.showLatexTableContent: HtmlEvent"

-- find a specific tag field in a list of HTML attributes:
findHtmlAttr :: String -> [(String,String)] -> Maybe String
findHtmlAttr _    [] = Nothing
findHtmlAttr atag ((t,f):attrs) =
  if atag==t then Just f
             else findHtmlAttr atag attrs


--- Convert special characters into TeX representation, if necessary.
specialchars2tex :: String -> String
specialchars2tex = htmlSpecialChars2tex . escapeLaTeXSpecials

escapeLaTeXSpecials :: String -> String
escapeLaTeXSpecials [] = []
escapeLaTeXSpecials (c:cs)
  | c=='^'      = "{\\tt\\char94}" ++ escapeLaTeXSpecials cs
  | c=='~'      = "{\\tt\\char126}" ++ escapeLaTeXSpecials cs
  | c=='\\'     = "{\\textbackslash}" ++ escapeLaTeXSpecials cs
  | c=='<'      = "{\\tt\\char60}" ++ escapeLaTeXSpecials cs
  | c=='>'      = "{\\tt\\char62}" ++ escapeLaTeXSpecials cs
  | c=='_'      = "\\_" ++ escapeLaTeXSpecials cs
  | c=='#'      = "\\#" ++ escapeLaTeXSpecials cs
  | c=='$'      = "\\$" ++ escapeLaTeXSpecials cs
  | c=='%'      = "\\%" ++ escapeLaTeXSpecials cs
  | c=='{'      = "\\{" ++ escapeLaTeXSpecials cs
  | c=='}'      = "\\}" ++ escapeLaTeXSpecials cs
  | otherwise   = c : escapeLaTeXSpecials cs

--- Convert special HTML characters into their LaTeX representation,
--- if necessary.
htmlSpecialChars2tex :: String -> String
htmlSpecialChars2tex [] = []
htmlSpecialChars2tex (c:cs)
  | c==chr 228  = "{\\\"a}"  ++ htmlSpecialChars2tex cs
  | c==chr 246  = "{\\\"o}"  ++ htmlSpecialChars2tex cs
  | c==chr 252  = "{\\\"u}"  ++ htmlSpecialChars2tex cs
  | c==chr 196  = "{\\\"A}"  ++ htmlSpecialChars2tex cs
  | c==chr 214  = "{\\\"O}"  ++ htmlSpecialChars2tex cs
  | c==chr 220  = "{\\\"U}"  ++ htmlSpecialChars2tex cs
  | c==chr 223  = "{\\ss}" ++ htmlSpecialChars2tex cs
  | c=='&'      = let (special,rest) = break (==';') cs
                  in  if null rest
                      then "\\&" ++ htmlSpecialChars2tex special -- wrong format
                      else htmlspecial2tex special ++
                           htmlSpecialChars2tex (tail rest)
  | otherwise   = c : htmlSpecialChars2tex cs

htmlspecial2tex :: String -> String
htmlspecial2tex special
  | special=="Auml"   =  "{\\\"A}"
  | special=="Euml"   =  "{\\\"E}"
  | special=="Iuml"   =  "{\\\"I}"
  | special=="Ouml"   =  "{\\\"O}"
  | special=="Uuml"   =  "{\\\"U}"
  | special=="auml"   =  "{\\\"a}"
  | special=="euml"   =  "{\\\"e}"
  | special=="iuml"   =  "{\\\"\\i}"
  | special=="ouml"   =  "{\\\"o}"
  | special=="uuml"   =  "{\\\"u}"
  | special=="szlig"  =  "{\\ss}"
  | special=="Aacute" =  "{\\\'A}"
  | special=="Eacute" =  "{\\\'E}"
  | special=="Iacute" =  "{\\\'I}"
  | special=="Oacute" =  "{\\\'O}"
  | special=="Uacute" =  "{\\\'U}"
  | special=="aacute" =  "{\\\'a}"
  | special=="eacute" =  "{\\\'e}"
  | special=="iacute" =  "{\\\'\\i}"
  | special=="oacute" =  "{\\\'o}"
  | special=="uacute" =  "{\\\'u}"
  | special=="Agrave" =  "{\\`A}"
  | special=="Egrave" =  "{\\`E}"
  | special=="Igrave" =  "{\\`I}"
  | special=="Ograve" =  "{\\`O}"
  | special=="Ugrave" =  "{\\`U}"
  | special=="agrave" =  "{\\`a}"
  | special=="egrave" =  "{\\`e}"
  | special=="igrave" =  "{\\`\\i}"
  | special=="ograve" =  "{\\`o}"
  | special=="ugrave" =  "{\\`u}"
  | special=="Acirc"  =  "{\\^A}"
  | special=="Ecirc"  =  "{\\^E}"
  | special=="Icirc"  =  "{\\^I}"
  | special=="Ocirc"  =  "{\\^O}"
  | special=="Ucirc"  =  "{\\^U}"
  | special=="acirc"  =  "{\\^a}"
  | special=="ecirc"  =  "{\\^e}"
  | special=="icirc"  =  "{\\^\\i}"
  | special=="ocirc"  =  "{\\^o}"
  | special=="ucirc"  =  "{\\^u}"
  | special=="Oslash" =  "{\\O}"
  | special=="oslash" =  "{\\o}"
  | special=="amp"    =  "{\\&}"
  | special=="ntilde" =  "{\\~n}"
  | special=="otilde" =  "{\\~o}"
  | special=="ccedil" =  "{\\c{c}}"
  | special=="nbsp"   =  "~"
  | special=="quot"   =  "\""
  | special=="lt"     =  "{$<$}"
  | special=="gt"     =  "{$>$}"
  | otherwise = "\\&"++special++";"

------------------------------------------------------------------------------
--- Transforms HTML expressions into a string representation of a complete
--- LaTeX document.

showLatexDoc :: [HtmlExp] -> String
showLatexDoc htmlexps = showLatexDocs [htmlexps]

--- Transforms HTML expressions into a string representation of a complete
--- LaTeX document.
--- The variable "packages" holds the packages to add to the latex document
--- e.g. "ngerman"

showLatexDocWithPackages :: [HtmlExp] -> [String] -> String
showLatexDocWithPackages hexps packages
  = showLatexDocsWithPackages [hexps] packages

--- Transforms a list of HTML expressions into a string representation
--- of a complete LaTeX document where each list entry appears
--- on a separate page.

showLatexDocs :: [[HtmlExp]] -> String
showLatexDocs htmlexps_list = showLatexDocsWithPackages htmlexps_list []


--- Transforms a list of HTML expressions into a string representation
--- of a complete LaTeX document where each list entry appears
--- on a separate page.
--- The variable "packages" holds the packages to add to the latex document
--- (e.g., "ngerman").

showLatexDocsWithPackages :: [[HtmlExp]] -> [String] -> String
showLatexDocsWithPackages htmlexps_list packages =
 "\\documentclass[12pt]{article}\n"++
 concatMap (\p->"\\usepackage{"++p++"}\n") packages++
 -- Package longtable is added by default.
 "\\usepackage{longtable}"++
 "\\nonstopmode\n"++
 "\\setlength{\\topmargin}{ -1.5cm}\n"++
 "\\setlength{\\oddsidemargin}{0.0cm}\n"++
 "\\setlength{\\evensidemargin}{0.0cm}\n"++
 "\\setlength{\\marginparwidth}{0.0cm}\n"++
 "\\setlength{\\marginparsep}{0.0cm}\n"++
 "\\setlength{\\textwidth}{16.5cm}\n"++
 "\\setlength{\\textheight}{24.0cm}\n"++
 "\\pagestyle{empty}\n"++
 "\\begin{document}\n\\sloppy\n"++
 "\\addtolength{\\baselineskip}{0.0ex}\n"++
 "\\setlength{\\parindent}{0.0ex}\n"++
 "\\addtolength{\\parskip}{0.5ex}\n"++
 intercalate "\\newpage\n" (map showLatexExps htmlexps_list) ++
 "\\end{document}\n"

--- show german latex document
germanLatexDoc :: [HtmlExp] -> String
germanLatexDoc hexps = showLatexDocWithPackages hexps ["ngerman"]

------------------------------------------------------------------------------
