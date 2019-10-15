{-
"Prog3Tests1.hs" - Test Cases for HW 3 -> Prog3.hs
Last Edited: 14 OCT 2019
West Chester University - CSC 345 - Programming Language Concepts / Paradigms - Fall 2019
Original format provided by: Richard Burns , distributed with permission.
Custom additions for Prog3 by: Mahmoud Gudarzi, Anton Adamovich, and Akash Kumar

AUTHORS GIVE NO GUARANTEES THAT TEST CASES ARE CORRECT OR COMPLETE.
INTRUCTOR HAS FINAL WORD CONCERNING THE FUNCTIONALITY OF YOUR CODE.
YOU ARE ENCOURAGED TO TEST YOUR CODE INDEPENDENTLY.

Usage: ghci Prog3Tests; main

Dependencies: cabal update
              cabal install tasty
              cabal install tasty-hunit
-}

import Prog3
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
        -- productLastPart :: Int -> [Int] -> Int
        testCase "test_productLastPart1"
            $ assertEqual [] 6 (productLastPart 2 [1,2,3]),
        testCase "test_productLastPart2"
            $ assertEqual [] 1 (productLastPart 1 [5,4,3,2,1]),
        testCase "test_productLastPart3"
            $ assertEqual [] 0 (productLastPart 0 [1,2,3,4,5]),
        testCase "test_productLastPart4"
            $ assertEqual [] 30 (productLastPart  2 [5,6]),
        testCase "test_productLastPart5"
            $ assertEqual [] (-24) (productLastPart 5 [2,2,-2,3,1]),

        -- init' :: [Int] -> [Int]
        testCase "test_init'1"
            $ assertEqual [] [1] (init' [1,2]),
        testCase "test_init'2"
            $ assertEqual [] [] (init' [1]),
        testCase "test_init'3"
            $ assertEqual [] [1,2,3,4] (init' [1,2,3,4,5]),

        -- init'' :: [Int] -> [Int]
        testCase "test_init''1"
            $ assertEqual [] [1] (init'' [1,2]),
        testCase "test_init''2"
            $ assertEqual [] [] (init'' [1]),
        testCase "test_init''3"
            $ assertEqual [] [1,2,3,4] (init'' [1,2,3,4,5]),

        -- elemAt :: Integer -> [Integer] -> Integer
        testCase "test_elemAt1"
            $ assertEqual [] 65 (elemAt 6 [1,1,1,1,1,65]),
        testCase "test_elemAt2"
            $ assertEqual [] 0 (elemAt 1 [0,1,2,3,4]),
        testCase "test_elemAt3"
            $ assertEqual [] 2 (elemAt 2 [1,2,99]),

        -- numTimes :: Int -> [Int] -> Int
        testCase "test_numTimes1"
            $ assertEqual [] 0 (numTimes 2 []),
        testCase "test_numTimes2"
            $ assertEqual [] 3 (numTimes 4 [4,3,3,3,4,4]),
        testCase "test_numTimes3"
            $ assertEqual [] 0 (numTimes 99 [1,2,3,4,5]),
        testCase "test_numTimes4"
            $ assertEqual [] 2 (numTimes (-42) [1,-42,3,1,1,42,-42]),

        -- lowerFirstLetter :: String -> String
        testCase "test_lowerFirstLetter1"
            $ assertEqual [] [] (lowerFirstLetter ""),
        testCase "test_lowerFirstLetter2"
            $ assertEqual [] "lower" (lowerFirstLetter "Lower" ),
        testCase "test_lowerFirstLetter3"
            $ assertEqual [] "lower" (lowerFirstLetter "lower"),
        testCase "test_lowerFirstLetter3"
            $ assertEqual [] " Lower" (lowerFirstLetter " Lower"),

        -- nestedParens :: String -> Bool
        testCase "test_nestedParens1"
            $ assertEqual [] True (nestedParens ""),
        testCase "test_nestedParens2"
            $ assertEqual [] False (nestedParens ")("),
        testCase "test_nestedParens3"
            $ assertEqual [] False (nestedParens "("),
        testCase "test_nestedParens4"
            $ assertEqual [] False (nestedParens "(()))"),
        testCase "test_nestedParens5"
            $ assertEqual [] True (nestedParens "((()))"),
        testCase "test_nestedParens6"
            $ assertEqual [] True (nestedParens "()"),

        -- triads :: Int -> [(Int,Int,Int)]
        testCase "test_triads1"
            $ assertEqual [] [(0,0,0),(0,1,1),(0,2,2),
                              (0,3,3),(1,0,1),(2,0,2),(3,0,3)]
                             (triads 3),
        testCase "test_triads2"
            $ assertEqual [] [(0,0,0),(0,1,1),(0,2,2),
                              (0,3,3),(0,4,4),(0,5,5),
                              (1,0,1),(2,0,2),(3,0,3),
                              (3,4,5),(4,0,4),(4,3,5),(5,0,5)]
                             (triads 5),
        testCase "test_triads3"
            $ assertEqual [] [(0,0,0),(0,1,1),(1,0,1)]
                             (triads 1),
        testCase "test_triads4"
            $ assertEqual [] [(0,0,0)] (triads 0),

        -- iSort' :: [(Float, Int, String)] -> [(Float, Int, String)]
        testCase "test_iSort'1"
            $ assertEqual [] [(0.3,3,"sorted")]
                             (iSort' [(0.3,3,"sorted")]),
        testCase "test_iSort'2"
            $ assertEqual [] [] (iSort' []),
        testCase "test_iSort'3"
            $ assertEqual [] [(0.1,1,"This"),(0.2,2,"is"),
                              (0.3,3,"sorted"),(0.4,4,"now"),
                              (0.5,5,"yay!")]
                             (iSort' [(0.3,3,"sorted"),(0.5,5,"yay!"),
                                      (0.2,2,"is"),(0.1,1,"This"),
                                      (0.4,4,"now")]),
        testCase "test_iSort'4"
            $ assertEqual [] [(0.1,1,"This"),(0.2,2,"is"),
                              (0.3,3,"already"),(0.4,4,"sorted"),
                              (0.5,5,"yay!")]
                             (iSort' [(0.1,1,"This"),(0.2,2,"is"),
                                      (0.3,3,"already"),(0.4,4,"sorted"),
                                      (0.5,5,"yay!")]),
        testCase "test_iSort'5"
            $ assertEqual [] [(0.1,0,"This"),(0.2,0,"sort"),
                              (0.3,0,"is"),(0.4,0,"stable"),
                              (0.5,0,"yay!")]
                             (iSort' [(0.1,0,"This"),(0.2,0,"sort"),
                                      (0.3,0,"is"),(0.4,0,"stable"),
                                      (0.5,0,"yay!")]),

        -- merge :: [Int] -> [Int] -> [Int]
        testCase "test_merge1"
            $ assertEqual [] [9,8,7,6,5,4,3,2,1,0]
                             (merge [9,7,5,3,1] [8,6,4,2,0]),
        testCase "test_merge2"
            $ assertEqual [] [9,8,7,6,4,-1,-2,-2,-13,-42]
                             (merge [9,7,-1,-2,-13] [8,6,4,-2,-42]),
        testCase "test_merge3"
            $ assertEqual [] [1,0] (merge [0] [1]),
        testCase "test_merge4"
            $ assertEqual [] [] (merge [] []),
        testCase "test_merge5"
            $ assertEqual [] [99] (merge [99] [])
    ]
