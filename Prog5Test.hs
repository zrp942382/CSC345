{-
"Prog5Test.hs" - Test Cases for HW 5 -> Prog5.hs
Last Edited: 21 OCT 2019
West Chester University
CSC 345 - Programming Language Concepts / Paradigms - Fall 2019
Original format provided by: Richard Burns, distributed with permission.
Authors: Mahmoud Gudarzi, Anton Adamovich, Brandon Barker, and Akash Kumar

AUTHORS GIVE NO GUARANTEES THAT TEST CASES ARE CORRECT OR COMPLETE.
INTRUCTOR HAS FINAL WORD CONCERNING THE FUNCTIONALITY OF YOUR CODE.
YOU ARE ENCOURAGED TO TEST YOUR CODE INDEPENDENTLY.

Usage: ghci Prog5Test; main

Dependencies: cabal update
              cabal install tasty
              cabal install tasty-hunit
-}

import Prog5
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
    -- 1. reverse' :: [a] -> [a]
    -- 2. isPalindrome :: String -> Bool
    -- 3. safeFindAfter :: String -> [String] -> Maybe [String]
    -- 4. member :: Char -> Set -> Bool
    -- 5. size :: Set -> Int
    -- 6. add :: Char -> Set -> Set
    -- 7. equal :: Set -> Set -> Bool
    -- 8. saferemove :: Char -> Set -> Maybe Set
    -- 9. union :: Set -> Set -> Set
    -- 10. intersection :: Set -> Set -> Set


    ]