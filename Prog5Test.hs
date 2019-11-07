{-
"Prog5Test.hs" - Test Cases for HW 5 -> Prog5.hs
West Chester University
CSC 345 - Programming Language Concepts / Paradigms - Fall 2019
Original format provided by: Richard Burns, distributed with permission.
Authors: Mahmoud Gudarzi, Anton Adamovich, Brandon Barker, and Akash Kumar

AUTHORS GIVE NO GUARANTEES THAT TEST CASES ARE CORRECT OR COMPLETE.
INSTRUCTOR HAS FINAL WORD CONCERNING THE FUNCTIONALITY OF YOUR CODE.
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


-- Do not Change -----
ins''' :: Char -> String -> String
ins''' x [] = [x]
ins''' x (y:ys)
  | x <= y = x:y:ys
  | otherwise = y: ins''' x ys

iSort''' :: String -> String
iSort''' [] = []
iSort''' (x : xs) = ins''' x (iSort''' xs)

instance Eq Set where
    Set a == Set b = iSort''' a == iSort''' b
    EmptySet == EmptySet = True
    _ == _ = False
    Set a /= Set b = not (Set a == Set b)
-----------------------------------------


main = do
    setEnv "TASTY_TIMEOUT" "2s"
    defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests = testGroup "Unit tests"
    [
        -- 1: reverse' :: [a] -> [a]
        testGroup "test_reverse'" [
            testCase "test_1" $ assertEqual [] "1" (reverse' "1"),
            testCase "test_2" $ assertEqual [] "987654321" (reverse' "123456789"),
            testCase "test_3" $ assertEqual [] "rahc" (reverse' "char"),
            testCase "test_4" $ assertEqual [] "madam" (reverse' "madam")],


        -- 2: isPalindrome :: String -> Bool
        testGroup "test_isPalindrome" [
            testCase "test_1" $ assertEqual [] True (isPalindrome "madam"),
            testCase "test_2" $ assertEqual [] True (isPalindrome "borroworrob"),
            testCase "test_3" $ assertEqual [] True (isPalindrome "anutforajaroftuna"),
            testCase "test_4" $ assertEqual [] False (isPalindrome "haskell"),
            testCase "test_5" $ assertEqual [] True (isPalindrome ""),
            testCase "test_6" $ assertEqual [] True (isPalindrome "A"),
            testCase "test_7" $ assertEqual [] True (isPalindrome "bb")],


        -- 3: safeFindAfter :: String -> [String] -> Maybe [String]
        testGroup "test_safeFindAfter" [
            testCase "test_1" $ assertEqual [] (Just ["cut","me"]) (safeFindAfter "you" ["why","would","you","cut","me"]),
            testCase "test_2" $ assertEqual [] (Just []) (safeFindAfter "1" ["1"]),
            testCase "test_3" $ assertEqual [] Nothing (safeFindAfter "what?" []),
            testCase "test_4" $ assertEqual [] (Just ["In","a","trap?"]) (safeFindAfter "clam?" ["Is","evil","in","a","clam?","In","a","trap?"]),
            testCase "test_5" $ assertEqual [] Nothing (safeFindAfter "a" ["b", "c"])],


        -- 4: member :: Char -> Set -> Bool
        testGroup "test_member" [
            testCase "test_1" $ assertEqual [] False (member 'a' (Set "b")),
            testCase "test_2" $ assertEqual [] False (member 'b' EmptySet),
            testCase "test_3" $ assertEqual [] True (member 'n' (Set "Ding")),
            testCase "test_4" $ assertEqual [] False (member '3' (Set "7215")),
            testCase "test_5" $ assertEqual [] True (member '3' (Set "3"))],


        -- 5: size :: Set -> Int
        testGroup "test_size" [
            testCase "test_1" $ assertEqual [] 0 (size EmptySet),
            testCase "test_2" $ assertEqual [] 22 (size (Set "whyareyoukeepingcount?")),
            testCase "test_3" $ assertEqual [] 1 (size (Set ['1'])),
            testCase "test_4" $ assertEqual [] 7 (size (Set "haskell"))],


        --6: add :: Char -> Set -> Set
        testGroup "test_add" [
            testCase "test_1" $ assertEqual [] (Set ['a']) (add 'a' EmptySet),
            testCase "test_2" $ assertEqual [] (Set "abcde") (add 'e' (Set "abcd")),
            testCase "test_3" $ assertEqual [] (Set "abcde") (add 'a' (Set "ecdb")),
            testCase "test_4" $ assertEqual [] (Set "1234567") (add '4' (Set "123567"))],

        -- 7: equal :: Set -> Set -> Bool
        testGroup "test_equal" [
            testCase "test_1" $ assertEqual [] True (equal EmptySet EmptySet),
            testCase "test_2" $ assertEqual [] False (equal EmptySet (Set "abc")),
            testCase "test_3" $ assertEqual [] True (equal (Set "123456789") (Set "987654321") ),
            testCase "test_4" $ assertEqual [] False (equal (Set "abc") EmptySet),
            testCase "test_5" $ assertEqual [] True (equal (Set "ghci") (Set "cigh")),
            testCase "test_6" $ assertEqual [] False (equal (Set "123456789") (Set "98765321"))],


        -- 8: saferemove :: Char -> Set -> Maybe Set
        testGroup "test_saferemove" [
            testCase "first_elem" $ assertEqual [] (Just (Set "bd")) (saferemove 'a' (Set "abd")),
            testCase "last_elem" $ assertEqual [] (Just (Set "1a34")) (saferemove 'd' (Set "134ad")),
            testCase "sinlge_elem" $ assertEqual []  (Just EmptySet) (saferemove '3' (Set "3")),
            testCase "remove_empty" $ assertEqual [] Nothing (saferemove '?' EmptySet),
            testCase "not_found" $ assertEqual [] Nothing (saferemove 'a' (Set "12gf")),
            testCase "middle_elem" $ assertEqual [] (Just (Set ",1a5b")) (saferemove ';' (Set "ab15;,"))],

        -- 9: union :: Set -> Set -> Set
        testGroup "test_union" [
            testCase "test_1" $ assertEqual [] (Set "Something") (union EmptySet (Set "Something")),
            testCase "test_2" $ assertEqual [] (Set "Something")  (union (Set "Something") EmptySet),
            testCase "test_3" $ assertEqual [] EmptySet (union EmptySet EmptySet),
            testCase "test_4" $ assertEqual [] (Set "123456789") (union (Set "123456") (Set "456789")),
            testCase "test_5" $ assertEqual [] (Set "12") (union (Set "1") (Set "2")),
            testCase "test_6" $ assertEqual [] (Set "1") (union (Set "1") (Set "1"))],

        -- 10: intersection :: Set -> Set -> Set
        testGroup "test_intersection" [
            testCase "test_1" $ assertEqual []  EmptySet (intersection EmptySet (Set "Something")),
            testCase "test_2" $ assertEqual [] EmptySet (intersection EmptySet EmptySet),
            testCase "test_3" $ assertEqual []  EmptySet (intersection (Set "Something") EmptySet),
            testCase "test_4" $ assertEqual []  (Set "Something") (intersection (Set "Something") (Set "Something")),
            testCase "test_5" $ assertEqual []  (Set "456") (intersection (Set "123456") (Set "456789")),
            testCase "test_6" $ assertEqual []  EmptySet (intersection (Set "12345") (Set "6789")),
            testCase "test_7" $ assertEqual []  (Set "1") (intersection (Set "1") (Set "1"))]
    ]
