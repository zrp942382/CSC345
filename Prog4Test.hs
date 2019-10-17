{-
"Prog4Test1.hs" - Test Cases for HW 4 -> Prog4.hs
Last Edited: 17 OCT 2019
West Chester University
CSC 345 - Programming Language Concepts / Paradigms - Fall 2019
Original format provided by: Richard Burns , distributed with permission.
Authors: Mahmoud Gudarzi, Anton Adamovich, Brandon Barker, and Akash Kumar

AUTHORS GIVE NO GUARANTEES THAT TEST CASES ARE CORRECT OR COMPLETE.
INTRUCTOR HAS FINAL WORD CONCERNING THE FUNCTIONALITY OF YOUR CODE.
YOU ARE ENCOURAGED TO TEST YOUR CODE INDEPENDENTLY.

Usage: ghci Prog4Test1; main

Dependencies: cabal update
              cabal inmorerecent
              cabal install tasty-hunit
-}

import Prog4
import Test.Tasty
import Test.Tasty.HUnit
import System.Environment

main = do
    setEnv "TASTY_TIMEOUT" "2s"
    defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests = testGroup "Unit tests"
    [
        -- 1: morerecent :: (Int,Int,Int)-> (Int,Int,Int) -> (Int,Int,Int)
        testCase "morerecent1" -- different day
            $ assertEqual [] (1,2,2019)
                             (morerecent (1,1,2019) (1,2,2019)),
        testCase "morerecent2" -- different month
            $ assertEqual [] (11,17,2019)
                             (morerecent (10,17,300) (11,17,300)),
        testCase "morerecent3" -- different year
            $ assertEqual [] (1,1,2008)
                             (morerecent (1,1,2000) (1,1,2008)),
        testCase "morerecent4" -- different everything
            $ assertEqual [] (3,5,1999)
                             (morerecent (3,5,1999) (12,31,1091)),

        -- 2: numInMonth :: [(Int,Int,Int)] -> Int -> Int
        testCase "test_numInMonth1"
            $ assertEqual [] 0 (numInMonth [(1,2,2019),(2,21,2016),
                                            (3,31,2013),(4,11,2011)] 6),
        testCase "test_numInMonth2"
            $ assertEqual [] 1 (numInMonth [(1,2,2019),(2,21,2016),
                                            (3,31,2013),(4,11,2011)] 2),
        testCase "test_numInMonth3"
            $ assertEqual [] 4 (numInMonth [(3,2,2019),(3,21,2016),
                                            (3,31,2013),(3,11,2011)] 3),

        -- 3: datesInMonth :: [(Int,Int,Int)] -> Int -> [(Int,Int,Int)]
        testCase "test_datesInMonth1"
            $ assertEqual [] []
                             (numInMonth [(1,2,2019),(2,21,2016),
                                          (3,31,2013),(4,11,2011)] 6),
        testCase "test_datesInMonth2"
            $ assertEqual [] [(2,21,2016)]
                             (numInMonth [(1,2,2019),(2,21,2016),
                                          (3,31,2013),(4,11,2011)] 2),
        testCase "test_datesInMonth3"
            $ assertEqual [] [(3,2,2019),(3,21,2016),
                              (3,31,2013),(3,11,2011)]
                             (numInMonth [(3,2,2019),(3,21,2016),
                                          (3,31,2013),(3,11,2011)] 3),

        -- 4: month2Str :: (Int,Int,Int) -> String
        testCase "test_month2Str1"
            $ assertEqual [] "January 1, 2019" (month2Str (1,1,2019)),
        testCase "test_month2Str2"
            $ assertEqual [] "July 4, 2000" (month2Str (7,4,2000)),
        testCase "test_month2Str3"
            $ assertEqual [] "December 22, 2022" (month2Str (12,22,2022)),
        testCase "test_month2Str4"
            $ assertEqual [] "May 25, 1992" (month2Str (5,25,1992)),

        -- 5: date2Str :: (Int,Int,Int) -> String
        testCase "test_date2Str1"
            $ assertEqual [] 0 (date2Str (1,1,2019)),
        testCase "test_date2Str2"
            $ assertEqual [] 3 (date2Str 4 [4,3,3,3,4,4]),
        testCase "test_date2Str3"
            $ assertEqual [] 0 (date2Str 99 [1,2,3,4,5]),
        testCase "test_date2Str4"
            $ assertEqual [] 2 (date2Str (-42) [1,-42,3,1,1,42,-42]),

        -- 6: monthLookup :: Int -> Int


        -- 7: monthRange :: Int -> Int -> [Int]


        -- 8: validDate :: (Int,Int,Int) -> Bool


        -- 9: validLeapDate :: (Int,Int,Int) -> Bool


        -- 10: season :: (Int,Int,Int) -> String

    ]
