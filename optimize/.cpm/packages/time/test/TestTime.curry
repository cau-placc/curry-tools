-----------------------------------------------------------------------------
-- A few tests for module Data.Time
-----------------------------------------------------------------------------

module TestTime where

import Test.Prop

import Data.Time

aTime :: CalendarTime
aTime = CalendarTime 2020 2 5 13 51 4 3600

testDayString :: Prop
testDayString = toDayString aTime -=- "February 5, 2020"

testTimeString :: Prop
testTimeString = toDayString aTime -=- "13:51:04"

testDaysOfMonth1 :: Prop
testDaysOfMonth1 = daysOfMonth 2 2020 -=- 29

testDaysOfMonth2 :: Prop
testDaysOfMonth2 = daysOfMonth 2 1900 -=- 28

testDaysOfMonth3 :: Prop
testDaysOfMonth3 = daysOfMonth 2 2000 -=- 29
