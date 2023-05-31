------------------------------------------------------------------------------
--- A simple library for graph visualization with
--- [Graphviz](http://www.graphviz.org/).
--- It provides a data structure to represent graphs and operations
--- to visualize them.
--- 
--- There is a convenient operation `viewDotGraph` which visualizes
--- a graph with the `dotviewcommand` specified in the rc file of
--- the Curry system. Thus, the value of the `dotviewcommand` field
--- in the rc file (e.g., `~/.pakcsrc` in case of PAKCS) should be
--- correctly defined. Here are some reasonable settings of this field:
--- 
---     dotviewcommand=dot -Tpdf > /tmp/dotxxx.pdf && xdg-open /tmp/dotxxx.pdf
---     dotviewcommand=neato -Tpdf > /tmp/dotxxx.pdf && xdg-open /tmp/dotxxx.pdf
---     dotviewcommand=circo -Tpdf > /tmp/dotxxx.pdf && xdg-open /tmp/dotxxx.pdf
---     dotviewcommand=fdp -Tpdf > /tmp/dotxxx.pdf && xdg-open /tmp/dotxxx.pdf
---
--- @author Michael Hanus
--- @version May 2023
------------------------------------------------------------------------------

module Data.GraphViz
  ( DotGraph, dgraph, dgraphWithAttrs, ugraph, ugraphWithAttrs
  , Node(..), Edge(..)
  , viewDotGraph, showDotGraph, showDotGraphWithAttrs
  , getDotViewCmd, setDotViewCmd )
 where

import Data.List         ( intercalate, last )
import System.IO         ( stderr, hClose, hPutStr, hPutStrLn )

import Data.PropertyFile ( getPropertyFromFile, updatePropertyFile )
import System.CurryPath  ( curryrcFileName )
import System.IOExts     ( connectToCommand )

------------------------------------------------------------------------------
-- Data types for graphs.

--- A Dot graph consists of a name, a list of graph attributes,
--- and lists of nodes and edges.
--- It can be either directed (`DGraph`) or undirected (`UGraph`).
data DotGraph = DGraph String [(String,String)] [Node] [Edge]
              | UGraph String [(String,String)] [Node] [Edge]

--- Constructs a directed graph from a name and a list of nodes and edges.
dgraph :: String -> [Node] -> [Edge] -> DotGraph
dgraph name nodes edges = DGraph name [] nodes edges

--- Constructs a directed graph from a name, a list of attributes,
--- and lists of nodes and edges.
--- The attributes are graph attributes of the DOT language, e.g.,
--- `[("ordering","out"), ("fontsize","10")]`.
dgraphWithAttrs :: String -> [(String,String)] -> [Node] -> [Edge] -> DotGraph
dgraphWithAttrs = DGraph

--- Constructs an undirected graph from a name and a list of nodes and edges.
ugraph :: String -> [Node] -> [Edge] -> DotGraph
ugraph name nodes edges = UGraph name [] nodes edges

--- Constructs an undirected graph from a name, a list of attributes,
--- and lists of nodes and edges.
--- The attributes are graph attributes of the DOT language, e.g.,
--- `[("ordering","out"), ("fontsize","10")]`.
ugraphWithAttrs :: String -> [(String,String)] -> [Node] -> [Edge] -> DotGraph
ugraphWithAttrs = UGraph

--- A node of a dot graph consists of a name and a list of attributes
--- for this node.
data Node = Node String [(String,String)]

--- An edge of a dot graph consists of the names of the source and target node
--- and a list of attributes for this edge.
data Edge = Edge String String [(String,String)]

------------------------------------------------------------------------------
--- Shows a Dot graph as a string of the DOT language.
showDotGraph :: DotGraph -> String
showDotGraph (DGraph name attrs nodes edges) =
  "digraph \"" ++ name ++ "\"" ++ graphbody2dot True attrs nodes edges
showDotGraph (UGraph name attrs nodes edges) =
  "graph \"" ++ name ++ "\"" ++ graphbody2dot False attrs nodes edges

graphbody2dot :: Bool -> [(String,String)] -> [Node] -> [Edge] -> String
graphbody2dot directed attrs nodes edges =
  "{\n" ++ concatMap attr2dot attrs
        ++ concatMap node2dot nodes
        ++ concatMap (edge2dot directed) edges ++ "}\n"

--- Deprecated. Use `dgraphWithAttrs` or `ugraphWithAttrs` to construct
--- graphs with attributes.
showDotGraphWithAttrs :: String -> DotGraph -> String
showDotGraphWithAttrs oldattrs dotgraph = case dotgraph of
  DGraph name attrs nodes edges ->
    "digraph \"" ++ name ++ "\"" ++ attrsbody2dot True attrs nodes edges
  UGraph name attrs nodes edges ->
    "graph \"" ++ name ++ "\"" ++ attrsbody2dot False attrs nodes edges
 where
  attrsbody2dot directed attrs nodes edges =
    "{\n" ++ (if null oldattrs then "" else oldattrs ++ "\n")
          ++ concatMap attr2dot attrs
          ++ concatMap node2dot nodes
          ++ concatMap (edge2dot directed) edges ++ "}\n"

attr2dot :: (String,String) -> String
attr2dot (name,value) =
  showDotID name ++ "=" ++ showDotID value ++ ";\n"

node2dot :: Node -> String
node2dot (Node nname attrs) =
  showDotID nname ++ showDotAttrs attrs ++ ";\n"

edge2dot :: Bool -> Edge -> String
edge2dot directed (Edge i j attrs) =
  showDotID i ++ edgeOp ++ showDotID j ++ showDotAttrs attrs ++ ";\n"
 where
  edgeOp = if directed then " -> " else " -- "

showDotAttrs :: [(String, String)] -> String
showDotAttrs attrs =
  if null attrs then ""
                else '[' : intercalate "," (map showDotAttr attrs) ++ "]"

--- Shows an attribute of a graph as a string of the DOT language.
--- If the attribute name is `label` and its value is enclosed in
--- angle brackets, it is shown as an HTML-like label, otherwise it is
--- enclosed in quotation marks.
showDotAttr :: (String,String) -> String
showDotAttr (name,value)
 | name == "label" && not (null value) && head value == '<' && last value == '>'
 = "label=" ++ value
 | otherwise
 = name ++ "=\"" ++ value ++ "\""

showDotID :: String -> String
showDotID s | all isAlphaNum s = s
            | otherwise        = '"' : concatMap escapeDQ s ++ "\""
 where
  escapeDQ c = if c=='"' then "\\\"" else [c]

------------------------------------------------------------------------------
--- Visualize a DOT graph with the `dotviewcommand` specified in
--- the rc file of the Curry system.
viewDotGraph :: DotGraph -> IO ()
viewDotGraph = viewDot . showDotGraph

--- Visualize a string of the DOT langugage with the `dotviewcommand`
--- from the rc file of the Curry system.
viewDot :: String -> IO ()
viewDot dottxt = do
    dotview <- getDotViewCmd
    if null dotview
      then hPutStrLn stderr
             "No definition for 'dotviewcommand' found in rc file"
      else do
        dotstr <- connectToCommand dotview
        hPutStr dotstr dottxt
        hClose dotstr

------------------------------------------------------------------------------
--- Read the command for viewing dot files from the rc file of the
--- Curry system.
getDotViewCmd :: IO String
getDotViewCmd = do
  rcfile <- curryrcFileName
  getPropertyFromFile rcfile "dotviewcommand" >>= return . maybe "" id

--- Sets the command for viewing dot files in the rc file of the
--- Curry system.
setDotViewCmd :: String -> IO ()
setDotViewCmd dvcmd = do
  rcfile <- curryrcFileName
  updatePropertyFile rcfile "dotviewcommand" dvcmd

------------------------------------------------------------------------------
