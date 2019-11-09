{- Test Cases for HW 6 -> Prog6.hs
West Chester University
CSC 345 - Programming Language Concepts / Paradigms - Fall 2019
Original format provided by: Richard Burns, distributed with permission.
Authors: Mahmoud Gudarzi, Anton Adamovich, Brandon Barker, and Akash Kumar
Contributors: Cole Gottdank
AUTHORS GIVE NO GUARANTEES THAT TEST CASES ARE CORRECT OR COMPLETE.
INSTRUCTOR HAS FINAL WORD CONCERNING THE FUNCTIONALITY OF YOUR CODE.
YOU ARE ENCOURAGED TO TEST YOUR CODE INDEPENDENTLY.
Usage: ghci Prog6Test; main
Dependencies: cabal update
              cabal install tasty
              cabal install tasty-hunit
-}

import Prog6
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
        -- 1: preorder :: Tree1 -> [Int]
        testGroup "test_preorder" [
            testCase "test_1" $ assertEqual [] [1] (preorder (Leaf1 1)),
            testCase "test_2" $ assertEqual [] [1,2,3] (preorder (Node1 1 (Leaf1 2) (Leaf1 3))),
            testCase "test_3" $ assertEqual [] [1,2,3,4,5,6,7] (preorder (Node1 1 (Node1 2 (Leaf1 3) (Leaf1 4)) (Node1 5 (Leaf1 6) (Leaf1 7)))),
            testCase "test_4" $ assertEqual [] [1,2,3,4,5,6,7,8,9] (preorder (Node1 1 (Node1 2 (Node1 3 (Leaf1 4) (Leaf1 5)) (Leaf1 6)) (Node1 7 (Leaf1 8) (Leaf1 9))))
        ], 
      
        -- 2: postorder :: Tree1 -> [Int]
        testGroup "test_postorder" [
            testCase "test_1" $ assertEqual [] [1] (postorder (Leaf1 1)),
            testCase "test_2" $ assertEqual [] [1,2,3] (postorder (Node1 3 (Leaf1 1) (Leaf1 2))),
            testCase "test_3" $ assertEqual [] [1,2,3,4,5,6,7] (postorder (Node1 7 (Node1 3 (Leaf1 1) (Leaf1 2)) (Node1 6 (Leaf1 4) (Leaf1 5)))),
            testCase "test_4" $ assertEqual [] [1,2,3,4,5,6,7,8,9] (postorder (Node1 9 (Node1 5 (Node1 3 (Leaf1 1) (Leaf1 2)) (Leaf1 4)) (Node1 8 (Leaf1 6) (Leaf1 7))))
        ],
         
         -- 3: sumPositives :: Tree1 -> Int 
        testGroup "test_sumPositives" [
            testCase "test_1" $ assertEqual [] 1 (sumPositives (Leaf1 1)),
            testCase "test_2" $ assertEqual [] 6 (sumPositives (Node1 3 (Leaf1 1) (Leaf1 2))),
            testCase "test_3" $ assertEqual [] 28 (sumPositives (Node1 7 (Node1 3 (Leaf1 1) (Leaf1 2)) (Node1 6 (Leaf1 4) (Leaf1 5)))),
            testCase "test_4" $ assertEqual [] 29 (sumPositives (Node1 9 (Node1 (-5) (Node1 3 (Leaf1 1) (Leaf1 2)) (Leaf1 (-1))) (Node1 8 (Leaf1 6) (Leaf1 (-2)))))
        ], 
 
         -- 4: countLeaves :: Tree1 -> Int
        testGroup "test_countLeaves" [
            testCase "test_1" $ assertEqual [] 1 (countLeaves (Leaf1 1)),
            testCase "test_2" $ assertEqual [] 2 (countLeaves (Node1 3 (Leaf1 1) (Leaf1 2))),
            testCase "test_3" $ assertEqual [] 4 (countLeaves (Node1 7 (Node1 3 (Leaf1 1) (Leaf1 2)) (Node1 6 (Leaf1 4) (Leaf1 5)))),
            testCase "test_4" $ assertEqual [] 5 (countLeaves (Node1 9 (Node1 5 (Node1 3 (Leaf1 1) (Leaf1 2)) (Leaf1 4)) (Node1 8 (Leaf1 6) (Leaf1 7))))
        ], 

        -- 5: depth :: Tree1 -> Int
        testGroup "test_depth" [
            testCase "test_1" $ assertEqual [] 0 (depth (Leaf1 1)),
            testCase "test_2" $ assertEqual [] 1 (depth (Node1 3 (Leaf1 1) (Leaf1 2))),
            testCase "test_3" $ assertEqual [] 2 (depth ( Node1 7 (Node1 3 (Leaf1 1) (Leaf1 2)) (Node1 6 (Leaf1 4) (Leaf1 5)) )),
            testCase "test_4" $ assertEqual [] 3 (depth ( Node1 9 (Node1 5 (Node1 3 (Leaf1 1) (Leaf1 2)) (Leaf1 4)) (Node1 8 (Leaf1 6) (Leaf1 7)) ))
        ], 

        -- 6: occurs :: Eq a => a -> Tree2 a -> Bool
        testGroup "test_occurs" [
            testCase "test_1" $ assertEqual [] False (occurs 9 (Leaf2 10)),
            testCase "test_2" $ assertEqual [] True (occurs 'a' (Node2 [(Leaf2 'b'), (Node2 [(Leaf2 'z'), (Leaf2 'a')]), (Node2 [(Leaf2 'c'), (Node2 [(Leaf2 'd'), (Leaf2 'f')])])])),
            testCase "test_3" $ assertEqual [] False (occurs 'e' (Node2 [(Leaf2 'b'), (Node2 [(Leaf2 'z'), (Leaf2 'a')]), (Node2 [(Leaf2 'c'), (Node2 [(Leaf2 'd'), (Leaf2 'f')])])])),
            testCase "test_4" $ assertEqual [] False (occurs "cool" (Node2 [(Leaf2 "my"), (Node2 [(Leaf2 "favorite"), Node2 [(Leaf2 "stuff")], (Leaf2 "things"), (Leaf2 "ever")]), (Node2 [(Leaf2 "are"), (Node2 [(Leaf2 "probably"), (Leaf2 "cookies")])])])),
            testCase "test_5" $ assertEqual [] True (occurs 10 (Node2 [(Node2 [(Node2 [(Leaf2 10)])])])),
            testCase "test_6" $ assertEqual [] True (occurs 500000 (Node2 [(Node2 [(Leaf2 100000), (Leaf2 200001), (Leaf2 200002), (Leaf2 200003), (Leaf2 200004), (Leaf2 200000), (Leaf2 500000)])])),
            testCase "test_7" $ assertEqual [] False (occurs True (Node2 [(Leaf2 False), (Leaf2 False), (Leaf2 False)]))
        ]
        
        -- 10: depthK
        --testGroup "test_depthK" [
        --    testCase "test_1" $ assertEqual [] 0 (depthK ),
        --    testCase "test_2" $ assertEqual [] 1 (depthK ),
        --    testCase "test_3" $ assertEqual [] 2 (depthK ),
        --    testCase "test_4" $ assertEqual [] 3 (depthK )
        --]
   ]
