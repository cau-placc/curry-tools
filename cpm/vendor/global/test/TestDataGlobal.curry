------------------------------------------------------------------------------
--- Some tests for library Data.Global
---
--- To run all tests automatically by the currycheck tool, use the command:
--- "curry-check TestDataGlobal"
---
--- @author Michael Hanus
------------------------------------------------------------------------------

import Data.Global
import System.Process ( system )
import Test.Prop

------------------------------------------------------------------------------
-- Testing a simple integer temporary global entity:
points :: GlobalT Int
points = globalT "TestDataGlobal.points" (div 1 1)

rwglobal :: IO (Int,Int)
rwglobal = do
  v1 <- readGlobalT points
  writeGlobalT points 42
  v2 <- readGlobalT points
  return (v1,v2)

testSimpleIntReadGlobalWriteGlobal = rwglobal `returns` (1,42)

------------------------------------------------------------------------------
-- Testing a temporary global entity containing a list structure:
nats :: GlobalT [Int]
nats = globalT "TestDataGlobal.nats" []

listrwglobal :: IO ([Int],[Int])
listrwglobal = do
  writeGlobalT nats [1..5]
  v1 <- readGlobalT nats
  writeGlobalT nats (v1++v1)
  v2 <- readGlobalT nats
  return (v1,v2)

testSimpleIntlistReadGlobalWriteGlobal =
  listrwglobal `returns` ([1..5],[1..5]++[1..5])

------------------------------------------------------------------------------
-- Testing the interaction of two integer temporary global entities:

-- A type synonym for temporary integer globals:
type GTInt = GlobalT Int

gint1 :: GTInt
gint1 = globalT "TestDataGlobal.gint1" 0

gint2 :: GTInt
gint2 = globalT "TestDataGlobal.gint2" 42

rwglobals :: IO [Int]
rwglobals = do
  v1 <- readGlobalT gint1
  v2 <- readGlobalT gint2
  writeGlobalT gint2 99
  v3 <- readGlobalT gint1
  v4 <- readGlobalT gint2
  writeGlobalT gint1 (v4+1)
  v5 <- readGlobalT gint1
  v6 <- readGlobalT gint2
  return [v1,v2,v3,v4,v5,v6]

testReadWriteTwoTemporaryGlobals = rwglobals `returns` [0,42,0,99,100,99]

------------------------------------------------------------------------------
-- Testing a simple integer persistent global entity:
ppoints :: GlobalP Int
ppoints = globalPersistent "pointsstore" (3+4)

rwglobalp :: IO (Int,Int)
rwglobalp = do
  v1 <- readGlobalP ppoints
  writeGlobalP ppoints 42
  v2 <- readGlobalP ppoints
  return (v1,v2)

testPersistentIntReadGlobalWriteGlobal = rwglobalp `returns` (7,42)


-- finalize: clean
testCleanUp :: PropIO
testCleanUp = (system "rm -r pointsstore*") `returns` 0
