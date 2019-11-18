{- Test Cases for HW 7 -> Prog7.hs
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
    :load Prog7Test
    main
Dependencies:
    cabal update
    cabal install tasty
    cabal install tasty-hunit
-}

import Data.List
import Prog7
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
unitTests = testGroup "Unit tests"
  [
    -- 1: unique :: Eq a => [a] -> [a]

 testGroup "test_unique"
    [
    testCase "test_A1" $ assertEqual [] "123abc" (sort (unique "321abc123abc123")),
    testCase "test_A2" $ assertEqual [] "3" (sort (unique "3")),
    testCase "test_A3" $ assertEqual [] [1, 2, 3, 4, 5] (sort (unique [1, 2, 3, 4, 5])),
    testCase "test_A4" $ assertEqual [] [1, 2, 3, 4, 5] (sort (unique [1, 2, 3, 4, 5, 1])),
    testCase "test_A5" $ assertEqual [] "" (unique ""),
    testCase "test_A6" $ assertEqual [] [1, 2] (sort (unique [2, 1, 1, 1, 1, 1, 1]))
    ],

    -- 2: value1 :: Expr1 -> Int
    testGroup "test_value1"
    [
    testCase "test_B1" $ assertEqual [] 17 (value1 (Add1 (Add1 (Val1 5) (Val1 10)) (Sub1(Val1 5)(Val1 3)))),
    testCase "test_B2" $ assertEqual [] (-2) (value1 (Sub1 (Val1 20) (Val1 22))),
    testCase "test_B3" $ assertEqual [] 7 (value1 ((Val1 7))),
    testCase "test_B4" $ assertEqual [] 4 (value1 (Sub1 (Val1 0) (Sub1 (Add1 (Val1 3) (Val1 5)) (Val1 12)))),
    testCase "test_B5" $ assertEqual [] (-24) (value1 (Sub1 (Add1 (Val1 (-10)) (Val1 4)) (Add1 (Val1 (17)) (Val1 1))))
    ],

    -- 3 (#4 in PDF): value2 :: Expr2 -> Maybe Int
    testGroup "test_value2"
    [
    testCase "test_B1" $ assertEqual [] (Just 17) (value2 (Add2 (Add2 (Val2 5) (Val2 10)) (Sub2(Val2 5)(Val2 3)))),
    testCase "test_B2" $ assertEqual [] (Just (-2)) (value2 (Sub2 (Val2 20) (Val2 22))),
    testCase "test_B3" $ assertEqual [] (Just 7) (value2 ((Val2 7))),
    testCase "test_B4" $ assertEqual [] (Just 4) (value2 (Sub2 (Val2 0) (Sub2 (Add2 (Val2 3) (Val2 5)) (Val2 12)))),
    testCase "test_B5" $ assertEqual [] (Just (-24)) (value2 (Sub2 (Add2 (Val2 (-10)) (Val2 4)) (Add2 (Val2 (17)) (Val2 1)))),
    testCase "test_C1" $ assertEqual [] (Just (-23)) (value2 (Sub2 (Mult2 (Val2 (-10)) (Val2 2)) (Div2 (Val2 (17)) (Val2 5)))),
    testCase "test_C2" $ assertEqual [] (Just 3) (value2 (Div2 (Mult2 (Sub2 (Add2 (Val2 (-10)) (Val2 (8))) (Val2 (-17))) (Val2 4)) (Val2 20))),
    testCase "test_C3" $ assertEqual [] Nothing (value2 (Sub2 (Add2 (Val2 (-10)) (Val2 2)) (Div2 (Val2 (17)) (Val2 0)))),
    testCase "test_C4" $ assertEqual [] Nothing (value2 (Div2 (Mult2 (Val2 7) (Val2 3)) (Sub2 (Val2 (4)) (Add2 (Val2 1) (Val2 3))))),
    testCase "test_C5" $ assertEqual [] Nothing (value2 (Div2 (Val2 11) (Val2 0))),
    testCase "test_C6" $ assertEqual [] Nothing (value2 (Add2 (Add2 (Val2 (-10)) (Val2 2)) (Div2 (Val2 (17)) (Val2 0)))),
    testCase "test_C7" $ assertEqual [] Nothing (value2 (Mult2 (Add2 (Val2 (-10)) (Val2 2)) (Div2 (Val2 (17)) (Val2 0))))

   ],

    -- 4 (#5 in PDF): show :: Expr2 -> String
    testGroup "test_show"
    [
    testCase "test_B1" $ assertEqual [] "5 + 10 + 5 - 3" (show (Add2 (Add2 (Val2 5) (Val2 10)) (Sub2 (Val2 5)(Val2 3)))),
    testCase "test_B2" $ assertEqual [] "20 - 22" (show (Sub2 (Val2 20) (Val2 22))),
    testCase "test_B3" $ assertEqual [] "7" (show ((Val2 7))),
    testCase "test_B4" $ assertEqual [] "0 - 3 + 5 - 12" (show (Sub2 (Val2 0) (Sub2 (Add2 (Val2 3) (Val2 5)) (Val2 12)))),
    testCase "test_B5" $ assertEqual [] "-10 + 4 - 17 + 1" (show (Sub2 (Add2 (Val2 (-10)) (Val2 4)) (Add2 (Val2 (17)) (Val2 1))))
    ],

    --5 (#6 in PDF): piglatinize :: String -> String
    testGroup "test_piglatinize"
    [
    testCase "test1" $ assertEqual [] "opeknay" (piglatinize "knope"),
    testCase "test2" $ assertEqual [] "iddledeedeefay" (piglatinize "fiddledeedee"),
    testCase "test3" $ assertEqual [] "askellhay" (piglatinize "haskell"),
    testCase "test4" $ assertEqual [] "abstemiousyay" (piglatinize "abstemious"),
    testCase "test5" $ assertEqual [] "ocolatechay" (piglatinize "chocolate"),
    testCase "test6" $ assertEqual [] "irthdaybay" (piglatinize "birthday"),
    testCase "test7" $ assertEqual [] "oyay" (piglatinize "o")
    ],

  --6 (#7 in PDF): balanced :: Tree a -> Bool
    testGroup "test_balanced"
    [
    testCase "test1" $ assertEqual []  True (balanced (Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c'))),
    testCase "test2" $ assertEqual []  False (balanced (Node (Node (Node (Leaf 5) (Leaf 5)) (Leaf (-1))) (Leaf 6))),
    testCase "test3" $ assertEqual []  True (balanced (Node (Node (Node (Leaf 5) (Leaf 5)) (Leaf (-1))) (Node (Node (Leaf 5) (Leaf 5)) (Leaf (-1))))),
    testCase "test4" $ assertEqual []  False (balanced (Node (Node (Leaf 5) (Leaf 5)) (Node (Leaf (-1)) (Node (Leaf 10) (Node (Leaf 10) (Leaf 9)))))),
    testCase "test5" $ assertEqual []  True (balanced (Node (Node (Node (Leaf 5) (Leaf 5)) (Leaf (-1))) (Node (Node (Leaf 5) (Leaf 5)) (Node (Leaf (-1)) (Leaf 3))))),
    testCase "test6" $ assertEqual []  False (balanced (Node (Node (Node (Leaf 5) (Node (Leaf (-1)) (Leaf 3))) (Leaf (-1))) (Node (Node (Leaf 5) (Leaf 5)) (Node (Leaf (-1)) (Leaf 3))))),
    testCase "test7" $ assertEqual []  True (balanced (Leaf 1)),
    testCase "test8" $ assertEqual []  True (balanced (Node (Leaf 'a') (Leaf 'b')))
    ],


  -- 7 (#9 in PDF): bEval :: BExpr3 -> Bool
    testGroup "test_bEval"
    [
    testCase "test1" $ assertEqual []  True (bEval (Or (EqualTo (Val3 5) (Div3(Val3 15)(Val3 3))) (BoolLit True))),
    testCase "test2" $ assertEqual []  True (bEval (EqualTo (Val3 (-3)) (Sub3 (Val3 4) (Val3 7)))),
    testCase "test3" $ assertEqual []  True (bEval (LessThan (Val3 (-3)) (Sub3 (Val3 4) (Val3 6)))),
    testCase "test4" $ assertEqual []  False (bEval (BoolLit False)),
    testCase "test5" $ assertEqual []  True (bEval (BoolLit True)),
    testCase "test6" $ assertEqual []  False (bEval (Or (LessThan (Div3(If(EqualTo(Val3 2)(Add3(Val3 2)(Val3 1)))(If(Or(BoolLit True)(BoolLit False))(Val3 25)(Val3 2))(Val3 325))(Val3 25))(Val3 12)) (BoolLit False)))
    ],

  -- 8 (#10 in PDF): value3 :: Expr3 -> Maybe Int
  testGroup "test_value3"
    [
    testCase "test1" $ assertEqual []  (Just 34) (value3 (Mult3 (Add3 (Add3 (Val3 5) (Val3 10)) (Sub3(Val3 5)(Val3 3))) (Div3 (Val3 10)(Mult3(Val3 1)(Val3 5))))),
    testCase "test3" $ assertEqual []  (Just 3) (value3 (If (BoolLit True)(Add3 (Val3 1) (Val3 2))(Val3 4))),
    testCase "test4" $ assertEqual []  (Just 4) (value3 (If (BoolLit False)(Add3 (Val3 1) (Val3 2))(Val3 4))),
    testCase "test2" $ assertEqual []  Nothing (value3 (Mult3 (Add3 (Add3 (Val3 5) (Val3 10)) (Sub3(Val3 5)(Val3 3))) (Div3 (Val3 10)(Mult3(If (BoolLit False) (Val3 2) (Val3 0))(Val3 5))))),
    testCase "test5" $ assertEqual []  (Just 15) (value3 (Mult3 (If (LessThan (Add3 (Val3 2) (Val3 2)) (Val3 5)) (Div3 (Val3 6)(Val3 2)) (Val3 7)) (Val3 5))),
    testCase "test6" $ assertEqual []  (Just 35) (value3 (Mult3 (If (LessThan (Add3 (Val3 2) (Val3 6)) (Val3 5)) (Div3 (Val3 6)(Val3 2)) (Val3 7)) (Val3 5))),
    testCase "test7" $ assertEqual []  Nothing (value3 (Div3 (Val3 8)(Val3 0))),
    testCase "test8" $ assertEqual []  (Just 1) (value3 (Div3 (If (EqualTo (Val3 3) (Add3 (Val3 2)(Val3 1))) (If (Or (BoolLit True)(BoolLit False)) (Val3 25) (Val3 2)) (Val3 3)) (Val3 25))),
    testCase "test9" $ assertEqual []  (Just 13) (value3 (Div3 (If (EqualTo (Val3 2) (Add3 (Val3 2)(Val3 1))) (If (Or (BoolLit True)(BoolLit False)) (Val3 25) (Val3 2)) (Val3 325)) (Val3 25)))
    ]

  ]
