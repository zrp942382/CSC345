{- Test Cases for HW 8 -> Prog7=8.hs
West Chester University
CSC 345 - Programming Language Concepts / Paradigms - Fall 2019
Original format provided by: Richard Burns, distributed with permission.
Authors: Mahmoud Gudarzi, Anton Adamovich, Brandon Barker, Akash Kumar,
    and Cole Gottdank
AUTHORS GIVE NO GUARANTEES THAT TEST CASES ARE CORRECT OR COMPLETE.
INSTRUCTOR HAS FINAL WORD CONCERNING THE FUNCTIONALITY OF YOUR CODE.
YOU ARE ENCOURAGED TO TEST YOUR CODE INDEPENDENTLY.
Usage:
    ghci
    :load Prog8Test
    main
Dependencies:
    cabal update
    cabal install tasty
    cabal install tasty-hunit
-}

import Data.List
import Prog8
import System.Environment
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = do
    setEnv "TASTY_TIMEOUT" "2s"
    defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests" [
        -- 1: sumSqNeg :: [Int] -> Int
        testGroup "test_sumSqNeg" [
            testCase "test1" $ assertEqual [] 0 (sumSqNeg []),
            testCase "test2" $ assertEqual [] 0 (sumSqNeg [1]),
            testCase "test3" $ assertEqual [] 0 (sumSqNeg [1,2,3]),
            testCase "test4" $ assertEqual [] 14 (sumSqNeg [1,-2,-3,4,-1,9]),
            testCase "test5" $ assertEqual [] 133 (sumSqNeg [-2,-11,2,-2,2,-2,1000,100,10]),
            testCase "test6" $ assertEqual [] 875329 (sumSqNeg [11,-932,0,1,-12,14,5,-81])
        ],

        -- 2. containing :: Eq a => [a] -> [a] -> Bool
        testGroup "test_containing" [
            --testCase "test1" $ assertEqual [] True (containing [] []),
            testCase "test2" $ assertEqual [] True (containing [1] [1]),
            testCase "test3" $ assertEqual [] True (containing [] [1]),
            testCase "test4" $ assertEqual [] True (containing [1,2,3,4,42] [1,2,3,4,42]),
            testCase "test5" $ assertEqual [] True (containing [True,False] [True,False,True]),
            testCase "test6" $ assertEqual [] True (containing ['a','#','3','h'] ['a','#','3','h','y']),
            testCase "test7" $ assertEqual [] True (containing ["aa","#4","323","hello jELlO","yUm yUM"] ["aa","#4","323","hello jELlO","yUm yUM"]),
            testCase "test8" $ assertEqual [] False (containing ["imaginary"] ["real","rational"]),
            testCase "test9" $ assertEqual [] False (containing [1,2,3,4,5] [1,2,3,4]),
            testCase "test10" $ assertEqual [] False (containing [1,2,3,4,5] [2,3,4,5,95]),
            testCase "test11" $ assertEqual [] False (containing [1,2,3,4,5] [2,3,4,5]),
            testCase "test12" $ assertEqual [] False (containing [1,1,1] [1,1]),
            testCase "test13" $ assertEqual [] False (containing [1,2,3,4,5] [])
        ],

        -- 3. total :: (Int -> Int) -> [Int] -> Int
        testGroup "total" [
            testCase "test1" $ assertEqual [] 0 (total (* 2) []),
            testCase "test2" $ assertEqual [] 12 (total (* 2) [1,2,3]),
            testCase "test3" $ assertEqual [] 92 (total (+ 3) [11,15,12,42]),
            testCase "test4" $ assertEqual [] 68 (total (+ (-3)) [11,15,12,42]),
            testCase "test5" $ assertEqual [] 65 (total (`div` 2) [4,5,12,13,98]),
            testCase "test6" $ assertEqual [] 0 (total (`div` 200) [4,5,12,13,98]),
            testCase "test7" $ assertEqual [] 126 (total (^ 2) [3,6,9]),
            testCase "test8" $ assertEqual [] 44732898 (total (^ 2 ^ 3) [3,6,9]),
            testCase "test9" $ assertEqual [] (-23) (total (1 -) [2,3,5,7,11])
        ],

        -- 4.
        testGroup "test_containing'" [
            --testCase "test1" $ assertEqual [] True (containing [] []),
            testCase "test2" $ assertEqual [] True (containing' [1] [1]),
            testCase "test3" $ assertEqual [] True (containing' [] [1]),
            testCase "test4" $ assertEqual [] True (containing' [1,2,3,4,42] [1,2,3,4,42]),
            testCase "test5" $ assertEqual [] True (containing' [True,False] [True,False,True]),
            testCase "test6" $ assertEqual [] True (containing' ['a','#','3','h'] ['a','#','3','h','y']),
            testCase "test7" $ assertEqual [] True (containing' ["aa","#4","323","hello jELlO","yUm yUM"] ["aa","#4","323","hello jELlO","yUm yUM"]),
            testCase "test8" $ assertEqual [] False (containing' ["imaginary"] ["real","rational"]),
            testCase "test9" $ assertEqual [] False (containing' [1,2,3,4,5] [1,2,3,4]),
            testCase "test10" $ assertEqual [] False (containing' [1,2,3,4,5] [2,3,4,5,95]),
            testCase "test11" $ assertEqual [] False (containing' [1,2,3,4,5] [2,3,4,5]),
            testCase "test12" $ assertEqual [] False (containing' [1,1,1] [1,1]),
            testCase "test13" $ assertEqual [] False (containing' [1,2,3,4,5] [])
        ],

        -- 5 lengths :: [String] -> [Int]
        testGroup "test_lengths" [
            testCase "test1" $ assertEqual [] [] (lengths []),
            testCase "test2" $ assertEqual [] [0,0,0] (lengths ["","",""]),
            testCase "test3" $ assertEqual [] [4,2,4,3,4,3] (lengths ["This","is","Bob.","Bob","says","hi."]),
            testCase "test4" $ assertEqual [] [3,5,2,5,5] (lengths ["The","world","is","quiet","here."]),
            testCase "test5" $ assertEqual [] [182] (lengths ["Lopadotemachoselachogaleokranioleipsanodrimhypotrimmatosilphioparaomelitokatakechymenokichlepikossyphophattoperisteralektryonoptekephalliokigklopeleiolagoiosiraiobaphetraganopterygon"]),
            testCase "test6" $ assertEqual [] [1,0,2,3] (lengths ["1","","20","345"]),
            testCase "test7" $ assertEqual [] [4,7,10,18] (lengths ["$@#!","&%&#*@(","+__DWJ(H*@", "><POO?@)@()#&!_+-="])
        ]-- UNCOMMENT COMMA ,
    ]
