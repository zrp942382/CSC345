{-
"Prog4Test.hs" - Test Cases for HW 4 -> Prog4.hs
Last Edited: 17 OCT 2019
West Chester University
CSC 345 - Programming Language Concepts / Paradigms - Fall 2019
Original format provided by: Richard Burns, distributed with permission.
Authors: Mahmoud Gudarzi, Anton Adamovich, Brandon Barker, and Akash Kumar

AUTHORS GIVE NO GUARANTEES THAT TEST CASES ARE CORRECT OR COMPLETE.
INTRUCTOR HAS FINAL WORD CONCERNING THE FUNCTIONALITY OF YOUR CODE.
YOU ARE ENCOURAGED TO TEST YOUR CODE INDEPENDENTLY.

Usage: ghci Prog4Test; main

Dependencies: cabal update
              cabal install tasty
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
        testCase "morerecent1"
            $ assertEqual [] (1,2,2019) (morerecent (1,1,2019) (1,2,2019)),
        testCase "morerecent2"
            $ assertEqual [] (11,17,300) (morerecent (10,17,300) (11,17,300)),
        testCase "morerecent3"
            $ assertEqual [] (1,1,2008) (morerecent (1,1,2000) (1,1,2008)),
        testCase "morerecent4"
            $ assertEqual [] (3,5,1999) (morerecent (3,5,1999) (12,31,1091)),

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
                             (datesInMonth [(1,2,2019),(2,21,2016),
                                          (3,31,2013),(4,11,2011)] 6),
        testCase "test_datesInMonth2"
            $ assertEqual [] [(2,21,2016)]
                             (datesInMonth [(1,2,2019),(2,21,2016),
                                          (3,31,2013),(4,11,2011)] 2),
        testCase "test_datesInMonth3"
            $ assertEqual [] [(3,2,2019),(3,21,2016),
                              (3,31,2013),(3,11,2011)]
                             (datesInMonth [(3,2,2019),(3,21,2016),
                                          (3,31,2013),(3,11,2011)] 3),

        -- 4: month2Str :: (Int,Int,Int) -> String
        testCase "test_month2Str1"
            $ assertEqual [] "January" (month2Str (1,1,2019)),
        testCase "test_month2Str2"
            $ assertEqual [] "July" (month2Str (7,4,2000)),
        testCase "test_month2Str3"
            $ assertEqual [] "December" (month2Str (12,22,2022)),
        testCase "test_month2Str4"
            $ assertEqual [] "May" (month2Str (5,25,1992)),

        -- 5: date2Str :: (Int,Int,Int) -> String
        testCase "test_date2Str1"
            $ assertEqual [] "January 1, 2019" (date2Str (1,1,2019)),
        testCase "test_date2Str2"
            $ assertEqual [] "July 4, 2000" (date2Str (7,4,2000)),
        testCase "test_date2Str3"
            $ assertEqual [] "December 22, 2022" (date2Str (12,22,2022)),
        testCase "test_date2Str4"
            $ assertEqual [] "May 25, 1992" (date2Str (5,25,1992)),

        -- REMEMBER TO UNCOMMENT THE COMMA IN LINE 88.
        -- 6: monthLookup :: Int -> Int
        testGroup "test_monthLookupJanuary" [
            testCase "day_5" $ assertEqual [] 1 (monthLookup 5),
            testCase "day_1" $ assertEqual [] 1 (monthLookup 1),
            testCase "day_28" $ assertEqual [] 1 (monthLookup 28),
            testCase "day_31" $ assertEqual [] 1 (monthLookup 31)],
        testGroup "test_monthLookupFebruary" [
            testCase "day_57" $ assertEqual [] 2 (monthLookup 57),
            testCase "day_58" $ assertEqual [] 2 (monthLookup 58),
            testCase "day_49" $ assertEqual [] 2 (monthLookup 49),
            testCase "day_59" $ assertEqual [] 2 (monthLookup 59)],
        testGroup "test_monthLookupMarch" [
            testCase "day_86" $ assertEqual [] 3 (monthLookup 86),
            testCase "day_74" $ assertEqual [] 3 (monthLookup 74),
            testCase "day_61" $ assertEqual [] 3 (monthLookup 61),
            testCase "day_90" $ assertEqual [] 3 (monthLookup 90)],
        testGroup "test_monthLookupApril" [
            testCase "day_103" $ assertEqual [] 4 (monthLookup 103),
            testCase "day_114" $ assertEqual [] 4 (monthLookup 114),
            testCase "day_112" $ assertEqual [] 4 (monthLookup 112),
            testCase "day_120" $ assertEqual [] 4 (monthLookup 120)],
        testGroup "test_monthLookupMay" [
            testCase "day_124" $ assertEqual [] 5 (monthLookup 124),
            testCase "day_123" $ assertEqual [] 5 (monthLookup 123),
            testCase "day_150" $ assertEqual [] 5 (monthLookup 150),
            testCase "day_151" $ assertEqual [] 5 (monthLookup 151)],
        testGroup "test_monthLookupJune" [
            testCase "day_156" $ assertEqual [] 6 (monthLookup 156),
            testCase "day_157" $ assertEqual [] 6 (monthLookup 157),
            testCase "day_162" $ assertEqual [] 6 (monthLookup 162),
            testCase "day_181" $ assertEqual [] 6 (monthLookup 181)],
        testGroup "test_monthLookupJuly" [
            testCase "day_204" $ assertEqual [] 7 (monthLookup 204),
            testCase "day_205" $ assertEqual [] 7 (monthLookup 205),
            testCase "day_195" $ assertEqual [] 7 (monthLookup 195),
            testCase "day_212" $ assertEqual [] 7 (monthLookup 212)],
        testGroup "test_monthLookupAugust" [
            testCase "day_213" $ assertEqual [] 8 (monthLookup 213),
            testCase "day_220" $ assertEqual [] 8 (monthLookup 220),
            testCase "day_242" $ assertEqual [] 8 (monthLookup 242),
            testCase "day_243" $ assertEqual [] 8 (monthLookup 243)],
        testGroup "test_monthLookupSeptember" [
            testCase "day_266" $ assertEqual [] 9 (monthLookup 266),
            testCase "day_250" $ assertEqual [] 9 (monthLookup 250),
            testCase "day_271" $ assertEqual [] 9 (monthLookup 271),
            testCase "day_273" $ assertEqual [] 9 (monthLookup 273)],
        testGroup "test_monthLookupOctober" [
            testCase "day_299" $ assertEqual [] 10 (monthLookup 299),
            testCase "day_291" $ assertEqual [] 10 (monthLookup 291),
            testCase "day_297" $ assertEqual [] 10 (monthLookup 297),
            testCase "day_304" $ assertEqual [] 10 (monthLookup 304)],
        testGroup "test_monthLookupNovember" [
            testCase "day_318" $ assertEqual [] 11 (monthLookup 318),
            testCase "day_325" $ assertEqual [] 11 (monthLookup 325),
            testCase "day_317" $ assertEqual [] 11 (monthLookup 317),
            testCase "day_334" $ assertEqual [] 11 (monthLookup 334)],
        testGroup "test_monthLookupDecember" [
            testCase "day_358" $ assertEqual [] 12 (monthLookup 358),
            testCase "day_361" $ assertEqual [] 12 (monthLookup 361),
            testCase "day_336" $ assertEqual [] 12 (monthLookup 336),
            testCase "day_365" $ assertEqual [] 12 (monthLookup 365)],

        -- 7: monthRange :: Int -> Int -> [Int]
        testCase "test_monthRange1" $ assertEqual [] [2, 3] (monthRange 46 90),
        testCase "test_monthRange2" $ assertEqual [] [4, 5, 6, 7, 8] (monthRange 112 238),
        testCase "test_monthRange3" $ assertEqual [] [4, 5, 6, 7, 8, 9] (monthRange 103 270),
        testCase "test_monthRange4" $ assertEqual [] [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11] (monthRange 10 333),
        testCase "test_monthRange5" $ assertEqual [] [8, 9] (monthRange 222 246),
        testCase "test_monthRange6" $ assertEqual [] [7, 8, 9, 10] (monthRange 206 287),
        testCase "test_monthRange7" $ assertEqual [] [3, 4, 5, 6, 7, 8, 9, 10, 11, 12] (monthRange 87 360),
        testCase "test_monthRange8" $ assertEqual [] [3, 4, 5, 6, 7, 8, 9, 10, 11] (monthRange 75 318),
        testCase "test_monthRange9" $ assertEqual [] [3] (monthRange 63 85),
        testCase "test_monthRange10" $ assertEqual [] [2, 3, 4, 5] (monthRange 57 151),
        testCase "test_monthRange11" $ assertEqual [] [2, 3] (monthRange 44 65),
        testCase "test_monthRange12" $ assertEqual [] [1, 2, 3, 4, 5, 6, 7, 8] (monthRange 17 223),
        testCase "test_monthRange13" $ assertEqual [] [4, 5, 6, 7, 8, 9, 10, 11] (monthRange 94 322),
        testCase "test_monthRange14" $ assertEqual [] [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11] (monthRange 5 332),
        testCase "test_monthRange15" $ assertEqual [] [5, 6, 7, 8] (monthRange 134 234),
        testCase "test_monthRange16" $ assertEqual [] [3] (monthRange 80 83),
        testCase "test_monthRange17" $ assertEqual [] [3, 4, 5, 6, 7, 8] (monthRange 73 234),
        testCase "test_monthRange18" $ assertEqual [] [3, 4, 5, 6, 7, 8, 9, 10] (monthRange 83 279),
        testCase "test_monthRange19" $ assertEqual [] [6, 7, 8] (monthRange 168 224),
        testCase "test_monthRange20" $ assertEqual [] [3, 4, 5] (monthRange 81 134),
        testCase "test_monthRange21" $ assertEqual [] [5, 6, 7, 8, 9] (monthRange 142 268),
        testCase "test_monthRange22" $ assertEqual [] [2, 3, 4, 5, 6, 7, 8, 9] (monthRange 42 271),
        testCase "test_monthRange23" $ assertEqual [] [2, 3, 4, 5, 6, 7, 8, 9] (monthRange 32 265),
        testCase "test_monthRange24" $ assertEqual [] [6] (monthRange 154 161),
        testCase "test_monthRange25" $ assertEqual [] [5, 6] (monthRange 144 180),
        testCase "test_monthRange25" $ assertEqual [] [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12] (monthRange 1 365),

        -- 8: validDate :: (Int,Int,Int) -> Bool
        testGroup "test_validDateJanuary" [
            testCase "day_1" $ assertEqual [] True (validDate (1, 1, 2019)),
            testCase "day_8" $ assertEqual [] True (validDate (1, 8, 2019)),
            testCase "day_26" $ assertEqual [] True (validDate (1, 26, 2019)),
            testCase "day_31" $ assertEqual [] True (validDate (1, 31, 2019)),
            testCase "day_0" $ assertEqual [] False (validDate (1, 0, 2019)),
            testCase "day_32" $ assertEqual [] False (validDate (1, 32, 2019))],
        testGroup "test_validDateFebruary" [
            testCase "day_1" $ assertEqual [] True (validDate (2, 1, 2019)),
            testCase "day_24" $ assertEqual [] True (validDate (2, 24, 2019)),
            testCase "day_16" $ assertEqual [] True (validDate (2, 16, 2019)),
            testCase "day_28" $ assertEqual [] True (validDate (2, 28, 2019)),
            testCase "day_0" $ assertEqual [] False (validDate (2, 0, 2019)),
            testCase "day_29" $ assertEqual [] False (validDate (2, 29, 2019))],
        testGroup "test_validDateMarch" [
            testCase "day_1" $ assertEqual [] True (validDate (3, 1, 2019)),
            testCase "day_30" $ assertEqual [] True (validDate (3, 30, 2019)),
            testCase "day_10" $ assertEqual [] True (validDate (3, 10, 2019)),
            testCase "day_31" $ assertEqual [] True (validDate (3, 31, 2019)),
            testCase "day_0" $ assertEqual [] False (validDate (3, 0, 2019)),
            testCase "day_32" $ assertEqual [] False (validDate (3, 32, 2019))],
        testGroup "test_validDateApril" [
            testCase "day_1" $ assertEqual [] True (validDate (4, 1, 2019)),
            testCase "day_7" $ assertEqual [] True (validDate (4, 7, 2019)),
            testCase "day_22" $ assertEqual [] True (validDate (4, 22, 2019)),
            testCase "day_30" $ assertEqual [] True (validDate (4, 30, 2019)),
            testCase "day_0" $ assertEqual [] False (validDate (4, 0, 2019)),
            testCase "day_31" $ assertEqual [] False (validDate (4, 31, 2019))],
        testGroup "test_validDateMay" [
            testCase "day_1" $ assertEqual [] True (validDate (5, 1, 2019)),
            testCase "day_18" $ assertEqual [] True (validDate (5, 18, 2019)),
            testCase "day_2" $ assertEqual [] True (validDate (5, 2, 2019)),
            testCase "day_31" $ assertEqual [] True (validDate (5, 31, 2019)),
            testCase "day_0" $ assertEqual [] False (validDate (5, 0, 2019)),
            testCase "day_32" $ assertEqual [] False (validDate (5, 32, 2019))],
        testGroup "test_validDateJune" [
            testCase "day_1" $ assertEqual [] True (validDate (6, 1, 2019)),
            testCase "day_8" $ assertEqual [] True (validDate (6, 8, 2019)),
            testCase "day_10" $ assertEqual [] True (validDate (6, 10, 2019)),
            testCase "day_30" $ assertEqual [] True (validDate (6, 30, 2019)),
            testCase "day_0" $ assertEqual [] False (validDate (6, 0, 2019)),
            testCase "day_31" $ assertEqual [] False (validDate (6, 31, 2019))],
        testGroup "test_validDateJuly" [
            testCase "day_1" $ assertEqual [] True (validDate (7, 1, 2019)),
            testCase "day_27" $ assertEqual [] True (validDate (7, 27, 2019)),
            testCase "day_16" $ assertEqual [] True (validDate (7, 16, 2019)),
            testCase "day_31" $ assertEqual [] True (validDate (7, 31, 2019)),
            testCase "day_0" $ assertEqual [] False (validDate (7, 0, 2019)),
            testCase "day_32" $ assertEqual [] False (validDate (7, 32, 2019))],
        testGroup "test_validDateAugust" [
            testCase "day_1" $ assertEqual [] True (validDate (8, 1, 2019)),
            testCase "day_8" $ assertEqual [] True (validDate (8, 8, 2019)),
            testCase "day_25" $ assertEqual [] True (validDate (8, 25, 2019)),
            testCase "day_31" $ assertEqual [] True (validDate (8, 31, 2019)),
            testCase "day_0" $ assertEqual [] False (validDate (8, 0, 2019)),
            testCase "day_32" $ assertEqual [] False (validDate (8, 32, 2019))],
        testGroup "test_validDateSeptember" [
            testCase "day_1" $ assertEqual [] True (validDate (9, 1, 2019)),
            testCase "day_13" $ assertEqual [] True (validDate (9, 13, 2019)),
            testCase "day_4" $ assertEqual [] True (validDate (9, 4, 2019)),
            testCase "day_30" $ assertEqual [] True (validDate (9, 30, 2019)),
            testCase "day_0" $ assertEqual [] False (validDate (9, 0, 2019)),
            testCase "day_31" $ assertEqual [] False (validDate (9, 31, 2019))],
        testGroup "test_validDateOctober" [
            testCase "day_1" $ assertEqual [] True (validDate (10, 1, 2019)),
            testCase "day_10" $ assertEqual [] True (validDate (10, 10, 2019)),
            testCase "day_9" $ assertEqual [] True (validDate (10, 9, 2019)),
            testCase "day_31" $ assertEqual [] True (validDate (10, 31, 2019)),
            testCase "day_0" $ assertEqual [] False (validDate (10, 0, 2019)),
            testCase "day_32" $ assertEqual [] False (validDate (10, 32, 2019))],
        testGroup "test_validDateNovember" [
            testCase "day_1" $ assertEqual [] True (validDate (11, 1, 2019)),
            testCase "day_25" $ assertEqual [] True (validDate (11, 25, 2019)),
            testCase "day_2" $ assertEqual [] True (validDate (11, 2, 2019)),
            testCase "day_30" $ assertEqual [] True (validDate (11, 30, 2019)),
            testCase "day_0" $ assertEqual [] False (validDate (11, 0, 2019)),
            testCase "day_31" $ assertEqual [] False (validDate (11, 31, 2019))],
        testGroup "test_validDateDecember" [
            testCase "day_1" $ assertEqual [] True (validDate (12, 1, 2019)),
            testCase "day_17" $ assertEqual [] True (validDate (12, 17, 2019)),
            testCase "day_8" $ assertEqual [] True (validDate (12, 8, 2019)),
            testCase "day_31" $ assertEqual [] True (validDate (12, 31, 2019)),
            testCase "day_0" $ assertEqual [] False (validDate (12, 0, 2019)),
            testCase "day_32" $ assertEqual [] False (validDate (12, 32, 2019))],

        -- 9: validLeapDate :: (Int,Int,Int) -> Bool
        testGroup "test_validLeapDateJanuary" [
            testCase "Day 1" $ assertEqual [] True (validLeapDate (1, 1, 2020)),
            testCase "Day 23" $ assertEqual [] True (validLeapDate (1, 23, 2020)),
            testCase "Day 6" $ assertEqual [] True (validLeapDate (1, 6, 2020)),
            testCase "Day 31" $ assertEqual [] True (validLeapDate (1, 31, 2020)),
            testCase "Day 0" $ assertEqual [] False (validLeapDate (1, 0, 2020)),
            testCase "Day 32" $ assertEqual [] False (validLeapDate (1, 32, 2020))],
        testGroup "test_validLeapDateFebruary" [
            testCase "Day 1" $ assertEqual [] True (validLeapDate (2, 1, 2020)),
            testCase "Day 5" $ assertEqual [] True (validLeapDate (2, 5, 2020)),
            testCase "Day 8" $ assertEqual [] True (validLeapDate (2, 8, 2020)),
            testCase "Day 29" $ assertEqual [] True (validLeapDate (2, 29, 2020)),
            testCase "Day 0" $ assertEqual [] False (validLeapDate (2, 0, 2020)),
            testCase "Day 30" $ assertEqual [] False (validLeapDate (2, 30, 2020))],
        testGroup "test_validLeapDateMarch" [
            testCase "Day 1" $ assertEqual [] True (validLeapDate (3, 1, 2020)),
            testCase "Day 25" $ assertEqual [] True (validLeapDate (3, 25, 2020)),
            testCase "Day 4" $ assertEqual [] True (validLeapDate (3, 4, 2020)),
            testCase "Day 31" $ assertEqual [] True (validLeapDate (3, 31, 2020)),
            testCase "Day 0" $ assertEqual [] False (validLeapDate (3, 0, 2020)),
            testCase "Day 32" $ assertEqual [] False (validLeapDate (3, 32, 2020))],
        testGroup "test_validLeapDateApril" [
            testCase "Day 1" $ assertEqual [] True (validLeapDate (4, 1, 2020)),
            testCase "Day 5" $ assertEqual [] True (validLeapDate (4, 5, 2020)),
            testCase "Day 21" $ assertEqual [] True (validLeapDate (4, 21, 2020)),
            testCase "Day 30" $ assertEqual [] True (validLeapDate (4, 30, 2020)),
            testCase "Day 0" $ assertEqual [] False (validLeapDate (4, 0, 2020)),
            testCase "Day 31" $ assertEqual [] False (validLeapDate (4, 31, 2020))],
        testGroup "test_validLeapDateMay" [
            testCase "Day 1" $ assertEqual [] True (validLeapDate (5, 1, 2020)),
            testCase "Day 28" $ assertEqual [] True (validLeapDate (5, 28, 2020)),
            testCase "Day 27" $ assertEqual [] True (validLeapDate (5, 27, 2020)),
            testCase "Day 31" $ assertEqual [] True (validLeapDate (5, 31, 2020)),
            testCase "Day 0" $ assertEqual [] False (validLeapDate (5, 0, 2020)),
            testCase "Day 32" $ assertEqual [] False (validLeapDate (5, 32, 2020))],
        testGroup "test_validLeapDateJune" [
            testCase "Day 1" $ assertEqual [] True (validLeapDate (6, 1, 2020)),
            testCase "Day 22" $ assertEqual [] True (validLeapDate (6, 22, 2020)),
            testCase "Day 28" $ assertEqual [] True (validLeapDate (6, 28, 2020)),
            testCase "Day 30" $ assertEqual [] True (validLeapDate (6, 30, 2020)),
            testCase "Day 0" $ assertEqual [] False (validLeapDate (6, 0, 2020)),
            testCase "Day 31" $ assertEqual [] False (validLeapDate (6, 31, 2020))],
        testGroup "test_validLeapDateJuly" [
            testCase "Day 1" $ assertEqual [] True (validLeapDate (7, 1, 2020)),
            testCase "Day 11" $ assertEqual [] True (validLeapDate (7, 11, 2020)),
            testCase "Day 13" $ assertEqual [] True (validLeapDate (7, 13, 2020)),
            testCase "Day 31" $ assertEqual [] True (validLeapDate (7, 31, 2020)),
            testCase "Day 0" $ assertEqual [] False (validLeapDate (7, 0, 2020)),
            testCase "Day 32" $ assertEqual [] False (validLeapDate (7, 32, 2020))],
        testGroup "test_validLeapDateAugust" [
            testCase "Day 1" $ assertEqual [] True (validLeapDate (8, 1, 2020)),
            testCase "Day 21" $ assertEqual [] True (validLeapDate (8, 21, 2020)),
            testCase "Day 11" $ assertEqual [] True (validLeapDate (8, 11, 2020)),
            testCase "Day 31" $ assertEqual [] True (validLeapDate (8, 31, 2020)),
            testCase "Day 0" $ assertEqual [] False (validLeapDate (8, 0, 2020)),
            testCase "Day 32" $ assertEqual [] False (validLeapDate (8, 32, 2020))],
        testGroup "test_validLeapDateSeptember" [
            testCase "Day 1" $ assertEqual [] True (validLeapDate (9, 1, 2020)),
            testCase "Day 24" $ assertEqual [] True (validLeapDate (9, 24, 2020)),
            testCase "Day 10" $ assertEqual [] True (validLeapDate (9, 10, 2020)),
            testCase "Day 30" $ assertEqual [] True (validLeapDate (9, 30, 2020)),
            testCase "Day 0" $ assertEqual [] False (validLeapDate (9, 0, 2020)),
            testCase "Day 31" $ assertEqual [] False (validLeapDate (9, 31, 2020))],
        testGroup "test_validLeapDateOctober" [
            testCase "Day 1" $ assertEqual [] True (validLeapDate (10, 1, 2020)),
            testCase "Day 21" $ assertEqual [] True (validLeapDate (10, 21, 2020)),
            testCase "Day 13" $ assertEqual [] True (validLeapDate (10, 13, 2020)),
            testCase "Day 31" $ assertEqual [] True (validLeapDate (10, 31, 2020)),
            testCase "Day 0" $ assertEqual [] False (validLeapDate (10, 0, 2020)),
            testCase "Day 32" $ assertEqual [] False (validLeapDate (10, 32, 2020))],
        testGroup "test_validLeapDateNovember" [
            testCase "Day 1" $ assertEqual [] True (validLeapDate (11, 1, 2020)),
            testCase "Day 18" $ assertEqual [] True (validLeapDate (11, 18, 2020)),
            testCase "Day 14" $ assertEqual [] True (validLeapDate (11, 14, 2020)),
            testCase "Day 30" $ assertEqual [] True (validLeapDate (11, 30, 2020)),
            testCase "Day 0" $ assertEqual [] False (validLeapDate (11, 0, 2020)),
            testCase "Day 31" $ assertEqual [] False (validLeapDate (11, 31, 2020))],
        testGroup "test_validLeapDateDecember" [
            testCase "Day 1" $ assertEqual [] True (validLeapDate (12, 1, 2020)),
            testCase "Day 26" $ assertEqual [] True (validLeapDate (12, 26, 2020)),
            testCase "Day 4" $ assertEqual [] True (validLeapDate (12, 4, 2020)),
            testCase "Day 31" $ assertEqual [] True (validLeapDate (12, 31, 2020)),
            testCase "Day 0" $ assertEqual [] False (validLeapDate (12, 0, 2020)),
            testCase "Day 32" $ assertEqual [] False (validLeapDate (12, 32, 2020))],

        -- 10: season :: (Int,Int,Int) -> String
        testGroup "test_validSeasonSpring" [
            testCase "(3, 20, 2019)" $ assertEqual [] "Spring" (season (3, 20, 2019)),
            testCase "(6, 20, 2019)" $ assertEqual [] "Spring" (season (6, 20, 2019)),
            testCase "(3, 31, 2019)" $ assertEqual [] "Spring" (season (3, 31, 2019)),
            testCase "(5, 28, 2019)" $ assertEqual [] "Spring" (season (5, 28, 2019))],
        testGroup "test_validSeasonSummer" [
            testCase "(6, 21, 2019)" $ assertEqual [] "Summer" (season (6, 21, 2019)),
            testCase "(9, 22, 2019)" $ assertEqual [] "Summer" (season (9, 22, 2019)),
            testCase "(6, 28, 2019)" $ assertEqual [] "Summer" (season (6, 28, 2019)),
            testCase "(9, 7, 2019)" $ assertEqual [] "Summer" (season (9, 7, 2019))],
        testGroup "test_validSeasonFall" [
            testCase "(9, 23, 2019)" $ assertEqual [] "Fall" (season (9, 23, 2019)),
            testCase "(12, 21, 2019)" $ assertEqual [] "Fall" (season (12, 21, 2019)),
            testCase "(10, 11, 2019)" $ assertEqual [] "Fall" (season (10, 11, 2019)),
            testCase "(11, 10, 2019)" $ assertEqual [] "Fall" (season (11, 10, 2019))],
        testGroup "test_validSeasonWinter" [
            testCase "(12, 22, 2019)" $ assertEqual [] "Winter" (season (12, 22, 2019)),
            testCase "(3, 19, 2020)" $ assertEqual [] "Winter" (season (3, 19, 2020)),
            testCase "(3, 13, 2020)" $ assertEqual [] "Winter" (season (3, 13, 2020)),
            testCase "(3, 15, 2020)" $ assertEqual [] "Winter" (season (3, 13, 2020))]
    ]