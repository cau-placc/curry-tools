-- | A pretty-printing library for laying out text in two dimensions.
--   It is adapted from the Haskell boxes library by Brent Yorgey.

module Boxes where

import Data.List ( intersperse, sum, transpose )

-- | A box has a defined size (rows x cols) and some content.
data Box = Box
  { rows :: Int
  , cols :: Int
  , content :: Content }

-- | Box alignment.
data Alignment = AlignFirst   -- ^ align at top/left
               | AlignCenter1 -- ^ centered, but biased to top/left
               | AlignCenter2 -- ^ centered, but biased to bottom/right
               | AlignLast    -- ^ align at bottom/right

-- | Top alignment.
top :: Alignment
top = AlignFirst

-- | Botton alignment.
bottom :: Alignment
bottom = AlignLast

-- | Left alignment.
left :: Alignment
left = AlignFirst

-- | Right alignment.
right :: Alignment
right = AlignLast

-- | Center-top/left alignment.
center1 :: Alignment
center1 = AlignCenter1

-- | Center-bottom/right alignment.
center2 :: Alignment
center2 = AlignCenter2

-- | Content of a box.
data Content = Blank                          -- ^ no content
             | Text String                    -- ^ a string
             | Row [Box]                      -- ^ a row of boxes
             | Col [Box]                      -- ^ a column of boxes
             | SubBox Alignment Alignment Box -- ^ an aligned subbox

-- | Creates an empty 0x0 box.
nullBox :: Box
nullBox = emptyBox 0 0

-- | Creates an empty box with the given size.
emptyBox :: Int -- ^ number of rows
         -> Int -- ^ number of columns
         -> Box
emptyBox r c = Box r c Blank

-- | Creates a 1x1 box from a character.
char :: Char -> Box
char c = Box 1 1 (Text [c])

-- | Creates a Nx1 box from a string of length N.
text :: String -> Box
text t = Box 1 (length t) (Text t)

-- | Combine two boxes horizontally with top alignment.
(<>) :: Box -> Box -> Box
l <> r = hcat top [l, r]

-- | Combine two boxes horizontally with top alignment and leave one column
--   between the boxes.
(<+>) :: Box -> Box -> Box
l <+> r = hcat top [l, emptyBox 0 1, r]

-- | Combine two boxes vertically with left alignment.
(//) :: Box -> Box -> Box
t // b = vcat left [t, b]

-- | Combine two boxes vertically with left alignment and leave one row between
--   the boxes.
(/+/) :: Box -> Box -> Box
t /+/ b = vcat left [t, emptyBox 1 0, b]

-- | Combines a list of boxes horizontally with the given alignment.
hcat :: Alignment -> [Box] -> Box
hcat a bs = Box h w (Row $ map (alignVert a h) bs)
 where
  (w, h) = sumMax cols 0 rows bs

-- | Combines a list of boxes horizontally with the given alignment and space 
--   between all boxes.
hsep :: Int -> Alignment -> [Box] -> Box
hsep sep a bs = punctuateH a (emptyBox 0 sep) bs

-- | Combines a list of boxes vertically with the given alignment.
vcat :: Alignment -> [Box] -> Box
vcat a bs = Box h w (Col $ map (alignHoriz a w) bs)
 where
  (h, w) = sumMax rows 0 cols bs

-- | Calculate sum and maximum of a list.
sumMax :: Ord b => (a -> Int) -> b -> (a -> b) -> [a] -> (Int, b)
sumMax f defaultMax g as = foldr go (,) as 0 defaultMax
 where
  go a r n b = (r $! f a + n) $! g a `max` b

-- | Combines a list of boxes vertically with the given alignment and space
--   between all boxes.
vsep :: Int -> Alignment -> [Box] -> Box
vsep sep a bs = punctuateV a (emptyBox sep 0) bs

-- | Combine a list of boxes horizontally with the given alignment and a copy of 
--   the given box between each two boxes.
punctuateH :: Alignment -> Box -> [Box] -> Box
punctuateH a p bs = hcat a (intersperse p bs)

-- | Combine a list of boxes vertically with the given alignment and a copy of
--   the given box between each two boxes.
punctuateV :: Alignment -> Box -> [Box] -> Box
punctuateV a p bs = vcat a (intersperse p bs)

paraFill :: Alignment -> Int -> String -> Box
paraFill a n t = (\ss -> mkParaBoxFill a (length ss) n ss) $ flow n t

mkParaBoxFill :: Alignment -> Int -> Int -> [String] -> Box
mkParaBoxFill a h w = align AlignFirst a h w . vcat a . map text

-- | Create a box of the given width, containing a specific text. The text is
--   flowed to fit the width according to the alignment.
para :: Alignment -- ^ the alignment of the text
     -> Int       -- ^ the box's width
     -> String    -- ^ the box's contents
     -> Box
para a n t = (\ss -> mkParaBox a (length ss) ss) $ flow n t

-- | Creates a list of boxes, each of a specific width and height. The given 
--   text is flowed into as many columns as necessary according to the given
--   alignment.
columns :: Alignment -> Int -> Int -> String -> [Box]
columns a w h t = map (mkParaBox a h) . chunksOf h $ flow w t

-- | Creates a box of a specific height that contains a list of texts.
mkParaBox :: Alignment -> Int -> [String] -> Box
mkParaBox a n = alignVert top n . vcat a . map text

-- | Flows a given text into a given width, creating many different strings.
flow :: Int -> String -> [String]
flow n t = map (take n)
         . getLines
         $ foldl addWordP (emptyPara n) (map mkWord . words $ t)

-- | A paragraph has a width and some content.
data Para = Para
  { paraWidth :: Int
  , paraContent :: ParaContent }

-- | A paragraph's content is a block consisting of many full lines and a single
--   last line.
data ParaContent = Block
  { fullLines :: [Line]
  , lastLine :: Line }

-- | Creates an empty paragraph of the given width.
emptyPara :: Int -> Para
emptyPara pw = Para pw (Block [] (Line 0 []))

-- | Returns all lines of a paragraph.
getLines :: Para -> [String]
getLines (Para _ (Block ls l))
  | lLen l == 0 = process ls
  | otherwise   = process (l:ls)
  where process = map (unwords . reverse . map getWord . getWords) . reverse

-- | A line has a length and a list of words.
data Line = Line 
  { lLen :: Int
  , getWords :: [Word] }

-- | Creates a line from a list of words.
mkLine :: [Word] -> Line
mkLine ws = Line (sum (map ((+1) . wLen) ws) - 1) ws

-- | Creates a line from a single word.
startLine :: Word -> Line
startLine = mkLine . (:[])

-- | A word has a length and its contents.
data Word = Word
  { wLen :: Int
  , getWord :: String }

-- | Creates a word from a string.
mkWord :: String -> Word
mkWord w = Word (length w) w

-- | Adds a word to a paragraph.
addWordP :: Para -> Word -> Para
addWordP (Para pw (Block fl l)) w
  | wordFits pw w l = Para pw (Block fl (addWordL w l))
  | otherwise       = Para pw (Block (l:fl) (startLine w))

-- | Adds a word to a line.
addWordL :: Word -> Line -> Line
addWordL w (Line len ws) = Line (len + wLen w + 1) (w:ws)

-- | Checks whether a word fits into a line.
wordFits :: Int -> Word -> Line -> Bool
wordFits pw w l = lLen l == 0 || lLen l + wLen w + 1 <= pw

-- | Creates a box of a specific width containing another box's content aligned
--   according to the given alignment.
alignHoriz :: Alignment -> Int -> Box -> Box
alignHoriz a c b = align a AlignFirst (rows b) c b

-- | Creates a box of a specific height containing another box's content aligned
--   according to the given alignment.
alignVert :: Alignment -> Int -> Box -> Box
alignVert a r b = align AlignFirst a r (cols b) b

-- | Creates a box of a specific width and height containing another box's 
--   content aligned according to the given alignment.
align :: Alignment -> Alignment -> Int -> Int -> Box -> Box
align ah av r c = Box r c . SubBox ah av

-- | Move a box up by putting it into a larger box with extra rows, aligned to
--   the top. See remarks for moveLeft.
moveUp :: Int -> Box -> Box
moveUp n b = alignVert top (rows b + n) b

-- | Move a box down by putting it into a larger box with extra rows, aligned to
--   the bottom. See remarks for moveLeft.
moveDown :: Int -> Box -> Box
moveDown n b = alignVert bottom (rows b + n) b

-- | Move a box left by putting it into a larger box with extra columns, aligned
--   to the left. Note that this will only move the box by the specified amount 
--   if it is already in a larger right-aligned box.
moveLeft :: Int -> Box -> Box
moveLeft n b = alignHoriz left (cols b + n) b

-- | Move a box right by putting it into a larger box with extra columns, aligned
--   to the right. See remarks for moveRight.
moveRight :: Int -> Box -> Box
moveRight n b = alignHoriz right (cols b + n) b

-- | Create a table from a list of rows. A fixed width for each column must be
--   specified.
table :: [[String]] -> [Int] -> Box
table rows widths = vcat left $ map (hcat left . map (uncurry $ paraFill left)) withLengths
 where
  withLengths = map (zip widths) rows

-- | Render a box to a string.
render :: Box -> String
render = unlines . renderBox

-- | Takes a number of elements from a list. If the list is shorter than that 
--   number, fill the rest with a filler.
takeP :: a -> Int -> [a] -> [a]
takeP b n xs 
  | n <= 0 = []
  | otherwise = case xs of
    []     -> replicate n b
    (y:ys) -> y : takeP b (n - 1) ys

fReverse :: ([a], b) -> ([a], b)
fReverse (xs, y) = (reverse xs, y)

(***) :: (a -> b) -> (c -> d) -> ((a, c) -> (b, d))
f1 *** f2 = \(x, y) -> (f1 x, f2 y)

takePA :: Alignment -> a -> Int -> [a] -> [a]
takePA c b x = glue . (takeP b (numRev c x) *** takeP b (numFwd c x)) . split
 where
  split t = fReverse . splitAt (numRev c (length t)) $ t
  glue = uncurry (++) . fReverse
  numFwd AlignFirst n = n
  numFwd AlignLast  _ = 0
  numFwd AlignCenter1 n = n `div` 2
  numFwd AlignCenter2 n = (n + 1) `div` 2
  numRev AlignFirst _ = 0
  numRev AlignLast  n = n
  numRev AlignCenter1 n = (n + 1) `div` 2
  numRev AlignCenter2 n = n `div` 2

-- | Generates a string of spaces.
blanks :: Int -> String
blanks = flip replicate ' '

-- | Render a box as a list of lines.
renderBox :: Box -> [String]
renderBox (Box r c Blank) = resizeBox r c [""]
renderBox (Box r c (Text t)) = resizeBox r c [t]
renderBox (Box r c (Row bs)) = resizeBox r c
                             . merge
                             . map (renderBoxWithRows r)
                             $ bs
  where merge = foldr (zipWith (++)) (repeat [])
renderBox (Box r c (Col bs)) = resizeBox r c
                             . concatMap (renderBoxWithCols c)
                             $ bs
renderBox (Box r c (SubBox ha va b)) = resizeBoxAligned r c ha va
                                     . renderBox
                                     $ b

-- | Render a box as a list of lines with a given number of rows.
renderBoxWithRows :: Int -> Box -> [String]
renderBoxWithRows r b = renderBox (b { rows = r })

-- | Render a box as a list of lines with a given number of columns.
renderBoxWithCols :: Int -> Box -> [String]
renderBoxWithCols c b = renderBox (b { cols = c })

-- | Resize a rendered list of lines.
resizeBox :: Int -> Int -> [String] -> [String]
resizeBox r c = takeP (blanks c) r . map (takeP ' ' c)

-- | Resize a rendered list of lines using the given alignments.
resizeBoxAligned :: Int -> Int -> Alignment -> Alignment -> [String] -> [String]
resizeBoxAligned r c ha va = takePA va (blanks c) r . map (takePA ha ' ' c)

-- | Print a box to stdout.
printBox :: Box -> IO ()
printBox = putStr . render

-- From Haskell's Data.List.Split

chunksOf :: Int -> [a] -> [[a]]
chunksOf n xs = map (take n) (xs:(partials xs))
 where
  partials []       = []
  partials ys@(_:_) = let ys' = drop n ys in case ys' of
    []    -> []
    (_:_) -> (ys':(partials ys'))
