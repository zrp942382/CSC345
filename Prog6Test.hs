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
import System.Environment
import Data.List
import Test.Tasty
import Test.Tasty.HUnit

data Tree3 = Tree3 String [Tree3]

instance Show Tree3 where
  show = unlines . drawTree3
    where 
      drawTree3 :: Tree3 -> [String]
      drawTree3 (Tree3 x ts0) = x : drawSubTrees ts0
      drawSubTrees [] = []
      drawSubTrees [t] =
          "│" : shift "└ " "   " (drawTree3 t)
      drawSubTrees (t:ts) =
          "│" : shift "├ " "│  " (drawTree3 t) ++ drawSubTrees ts

      shift first other = zipWith (++) (first : repeat other)

instance Show x => Show (Tree2 x) where
  show t = show (tree2as3 t "[root]")
    where
      tree2as3 :: Show a => Tree2 a -> String -> Tree3
      tree2as3 (Leaf2 x) _       = Tree3 (show x) []
      tree2as3 (Node2 ns) spacer = Tree3 spacer [tree2as3 n "─┐" | n <- ns]

instance Show Tree1 where
  show = show . tree1as3
    where
      tree1as3 :: Tree1 -> Tree3
      tree1as3 (Leaf1 x)     = Tree3 (show x) []
      tree1as3 (Node1 x l r) = Tree3 (show x) [tree1as3 l, tree1as3 r]

assertTree :: (Eq o, Show t, Show o) => t -> o -> (t -> o)  -> Assertion
assertTree input output fn =
    assertEqual
        ("input tree: \n" ++ show input)
        output
        (fn input)

main :: IO ()
main = do
    setEnv "TASTY_TIMEOUT" "2s"
    defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests" [

        -- 1: preorder :: Tree1 -> [Int]
        testGroup "test_preorder" [
            testCase "test_1" $ assertTree (Leaf1 1) [1] preorder,

            testCase "test_2" $
                assertTree
                    (Node1 1 (Leaf1 2) (Leaf1 3))
                    [1,2,3]
                    preorder,

            testCase "test_3" $
                assertTree
                    (Node1 1
                       (Node1 2
                            (Leaf1 3)
                            (Leaf1 4))
                       (Node1 5
                            (Leaf1 6)
                            (Leaf1 7)))
                    [1,2,3,4,5,6,7]
                    preorder,                    


            testCase "test_4" $
                assertTree
                    (Node1 1
                       (Node1 2
                              (Node1 3
                                     (Leaf1 4)
                                     (Leaf1 5))
                              (Leaf1 6))
                       (Node1 7
                              (Leaf1 8)
                              (Leaf1 9)))
                    [1,2,3,4,5,6,7,8,9]
                    preorder
        ],

        -- 2: postorder :: Tree1 -> [Int]
        testGroup "test_postorder" [
            testCase "test_1" $ assertTree (Leaf1 1) [1] postorder,

            testCase "test_2" $
                assertTree
                    (Node1 1 (Leaf1 2) (Leaf1 3))
                    [2,3,1]
                    postorder,

            testCase "test_3" $
                assertTree
                    (Node1 1
                       (Node1 2
                            (Leaf1 3)
                            (Leaf1 4))
                       (Node1 5
                            (Leaf1 6)
                            (Leaf1 7)))
                    [3,4,2,6,7,5,1]
                    postorder,                    


            testCase "test_4" $
                assertTree
                    (Node1 1
                       (Node1 2
                              (Node1 3
                                     (Leaf1 4)
                                     (Leaf1 5))
                              (Leaf1 6))
                       (Node1 7
                              (Leaf1 8)
                              (Leaf1 9)))
                    [4,5,3,6,2,8,9,7,1]
                    postorder
        ],

        -- 3: sumPositives :: Tree1 -> Int
        testGroup "test_sumPositives" [
            testCase "test_1" $ assertTree (Leaf1 1) 1 sumPositives,

            testCase "test_2" $ assertTree (Leaf1 (-3)) 0 sumPositives,

            testCase "test_3" $ 
                assertTree 
                    (Node1 3
                        (Leaf1 1)
                        (Leaf1 2))
                    6
                    sumPositives,

            testCase "test_4" $
                assertTree 
                    (Node1 (-5)
                        (Leaf1 3)
                        (Leaf1 4))
                    7
                    sumPositives,


            testCase "test_5" $
                assertTree 
                    (Node1 (-5)
                        (Leaf1 (-9))
                        (Leaf1 100))
                    100
                    sumPositives,

            testCase "test_6" $
                assertTree 
                    (Node1 7
                        (Node1 3
                            (Leaf1 1)
                            (Leaf1 2))
                        (Node1 6
                            (Leaf1 4)
                            (Leaf1 5)))
                    28
                    sumPositives,

            testCase "test_7" $
                assertTree 
                    (Node1 9
                        (Node1 (-5)
                            (Node1 3
                                   (Leaf1 1)
                                   (Leaf1 2))
                            (Leaf1 (-1)))
                        (Node1 8
                            (Leaf1 6)
                            (Leaf1 (-2))))
                    29
                    sumPositives

        ],

        -- 4: countLeaves :: Tree1 -> Int
        testGroup "test_countLeaves" [
            testCase "test_1" $ assertTree (Leaf1 1) 1 countLeaves,
            
            testCase "test_2" $
                assertTree
                    (Node1 3
                        (Leaf1 1)
                        (Leaf1 2))
                    2
                    countLeaves,
            
            testCase "test_3" $
                assertTree
                    (Node1 7
                        (Node1 3
                               (Leaf1 1)
                               (Leaf1 2))
                        (Node1 6
                               (Leaf1 4)
                               (Leaf1 5)))
                    4
                    countLeaves,

            testCase "test_4" $
                assertTree
                    (Node1 9
                        (Node1 5
                               (Node1 3
                                      (Leaf1 1)
                                      (Leaf1 2))
                               (Leaf1 4))
                        (Node1 8
                               (Leaf1 6)
                               (Leaf1 7)))
                    5
                    countLeaves
        ],

        -- 5: depth :: Tree1 -> Int
        testGroup "test_depth" [
            testCase "test_1" $ assertTree (Leaf1 1) 0 depth,
            
            testCase "test_2" $
                assertTree
                    (Node1 3
                        (Leaf1 1)
                        (Leaf1 2))
                    1
                    depth,
            
            testCase "test_3" $
                assertTree
                    (Node1 7
                        (Node1 3
                               (Leaf1 1)
                               (Leaf1 2))
                        (Node1 6
                               (Leaf1 4)
                               (Leaf1 5)))
                    2
                    depth,

            testCase "test_4" $
                assertTree
                    (Node1 9
                        (Node1 5
                               (Node1 3
                                      (Leaf1 1)
                                      (Leaf1 2))
                               (Leaf1 4))
                        (Node1 8
                               (Leaf1 6)
                               (Leaf1 7)))
                    3
                    depth
        ],
        
        -- 6: occurs :: Eq a => a -> Tree2 a -> Bool
        testGroup "test_occurs" [
            testCase "test_1" $
                assertTree
                    (Leaf2 10)
                    False
                    (occurs 9),
           
            testCase "test_2" $
                assertTree
                    (Node2 [Leaf2 'b',
                        Node2 [Leaf2 'z',
                                Leaf2 'a'],
                        Node2 [Leaf2 'c',
                                Node2 [Leaf2 'd',
                                        Leaf2 'f']]])
                    True
                    (occurs 'a'),

            testCase "test_3" $
                assertTree
                    (Node2 [Leaf2 'b',
                        Node2 [Leaf2 'z',
                                Leaf2 'a'],
                        Node2 [Leaf2 'c',
                                Node2 [Leaf2 'd',
                                        Leaf2 'f']]])
                    False
                    (occurs 'e'),

            testCase "test_4" $
                assertTree
                    (Node2 [Leaf2 "my",
                            Node2 [Leaf2 "favorite",
                                   Node2 [Leaf2 "stuff"],
                                   Leaf2 "things",
                                   Leaf2 "ever"],
                            Node2 [Leaf2 "are",
                                   Node2 [Leaf2 "probably",
                                           Leaf2 "cookies"]]])
                    False
                    (occurs "cool"),


            testCase "test_5" $
                assertTree
                    (Node2 [Node2 [Node2 [Leaf2 10]]])
                    True
                    (occurs 10),

            testCase "test_6" $
                assertTree
                    (Node2 [Node2 [Leaf2 100000,
                                   Leaf2 200001,
                                   Leaf2 200002,
                                   Leaf2 200003,
                                   Leaf2 200004,
                                   Leaf2 200000,
                                   Leaf2 500000]])
                    True
                    (occurs 500000),

            testCase "test_7" $
                assertTree
                    (Node2 [Leaf2 False,
                            Leaf2 False,
                            Leaf2 False])
                    False
                    (occurs True)
        ],

        -- 7: countInteriorNodes :: Tree2 a -> Int
        testGroup "test_countInteriorNodes" [
            testCase "test_1" $
                assertTree
                    (Leaf2 10)
                    1
                    countInteriorNodes,
           
            testCase "test_2" $
                assertTree
                    (Node2 [Leaf2 'b',
                        Node2 [Leaf2 'z',
                                Leaf2 'a'],
                        Node2 [Leaf2 'c',
                                Node2 [Leaf2 'd',
                                        Leaf2 'f']]])
                    6
                    countInteriorNodes,

            testCase "test_3" $
                assertTree
                    (Node2 [Leaf2 'b',
                        Node2 [Leaf2 'z',
                                Leaf2 'a'],
                        Node2 [Leaf2 'c',
                                Node2 [Leaf2 'd',
                                        Leaf2 'f']]])
                    6
                    countInteriorNodes,

            testCase "test_4" $
                assertTree
                    (Node2 [Leaf2 "my",
                            Node2 [Leaf2 "favorite",
                                   Node2 [Leaf2 "stuff"],
                                   Leaf2 "things",
                                   Leaf2 "ever"],
                            Node2 [Leaf2 "are",
                                   Node2 [Leaf2 "probably",
                                           Leaf2 "cookies"]]])
                    8
                    countInteriorNodes,

            testCase "test_5" $
                assertTree
                    (Node2 [Node2 [Node2 [Leaf2 10]]])
                    1
                    countInteriorNodes,

            testCase "test_6" $
                assertTree
                    (Node2 [Node2 [Leaf2 100000,
                                   Leaf2 200001,
                                   Leaf2 200002,
                                   Leaf2 200003,
                                   Leaf2 200004,
                                   Leaf2 200000,
                                   Leaf2 500000]])
                    7
                    countInteriorNodes,

            testCase "test_7" $
                assertTree
                    (Node2 [Leaf2 False,
                            Leaf2 False,
                            Leaf2 False])
                    3
                    countInteriorNodes
        ],

        -- 8: sumTree :: Tree2 Int -> Int
        testGroup "test_sumTree" [
            testCase "test_1" $
                assertTree
                    (Leaf2 10)
                    10
                    sumTree,
           
            testCase "test_2" $
                assertTree
                    (Node2 [Leaf2 1,
                        Node2 [Leaf2 2,
                                Leaf2 3],
                        Node2 [Leaf2 4,
                                Leaf2 5]])
                    15
                    sumTree,

            testCase "test_3" $
                assertTree
                    (Node2 [Leaf2 1,
                            Node2 [Leaf2 2,
                                    Leaf2 3],
                            Node2 [Leaf2 4,
                                    Node2 [Leaf2 5,
                                            Leaf2 6]]])
                    21
                    sumTree,

            testCase "test_4" $
                assertTree
                    (Node2 [Leaf2 1,
                           Node2 [Leaf2 2,
                                   Node2 [Leaf2 3],
                                   Leaf2 4,
                                   Leaf2 5],
                           Node2 [Leaf2 6,
                                   Node2 [Leaf2 7,
                                           Leaf2 8]]])
                    36
                    sumTree,

            testCase "test_5" $
                assertTree
                    (Node2 [Node2 [Node2 [Leaf2 10]]])
                    10
                    sumTree,

            testCase "test_6" $
                assertTree
                    (Node2 [Node2 [Leaf2 1,
                                    Leaf2 2,
                                    Leaf2 3,
                                    Leaf2 4,
                                    Leaf2 5,
                                    Leaf2 6,
                                    Leaf2 7]])
                    28
                    sumTree,

            testCase "test_7" $
                assertTree
                    (Node2 [Leaf2 1, Leaf2 2, Leaf2 3])
                    6
                    sumTree
        ],
        -- 9: pre2 :: Tree2 a -> [a]
        testGroup "test_pre2" [
            testCase "test_1" $
                assertTree 
                    (Leaf2 10)
                    [10]
                    pre2,

            testCase "test_2" $
                assertTree 
                    (Node2 [Leaf2 1,
                        Node2 [Leaf2 2,
                               Leaf2 3],
                        Node2 [Leaf2 4,
                               Node2 [Leaf2 5,
                                        Leaf2 6]]])
                    [1,2,3,4,5,6]
                    pre2,

            testCase "test_3" $
                assertTree 
                    (Node2 [Leaf2 "my",
                        Node2 [Leaf2 "favorite",
                               Node2 [Leaf2 "stuff"],
                               Leaf2 "things",
                               Leaf2 "ever"],
                        Node2 [Leaf2 "are",
                               Node2 [Leaf2 "probably",
                                      Leaf2 "cookies"]]])
                    ["my","favorite","stuff","things","ever","are","probably","cookies"]
                    pre2,

            testCase "test_4" $
                assertTree
                    (Node2 [Node2 [Node2 [Leaf2 10]]])
                    [10]
                    pre2,

            testCase "test_5" $
                assertTree
                    (Node2 [Node2 [Leaf2 1,
                                    Leaf2 2,
                                    Leaf2 3,
                                    Leaf2 4,
                                    Leaf2 5,
                                    Leaf2 6,
                                    Leaf2 7]])
                    [1,2,3,4,5,6,7]
                    pre2,

            testCase "test_6" $
                assertTree
                    (Node2 [Leaf2 1,
                            Leaf2 2,
                            Leaf2 3])
                    [1,2,3]
                    pre2
        ],

        testGroup "test_depthK" [
            testCase "test_1" $
                assertTree
                    (Leaf2 10)
                    [10]
                    (sort . depthK 0),

            testCase "test_2" $
                assertTree
                    (Leaf2 10)
                    []
                    (sort . depthK 1),

            testCase "test_3" $
                assertTree 
                    (Node2 [Leaf2 1,
                        Node2 [Leaf2 2,
                               Leaf2 3],
                        Node2 [Leaf2 4,
                               Node2 [Leaf2 5,
                                      Leaf2 6]]])
                    [2,3,4]
                    (sort . depthK 2),

            testCase "test_4" $
                assertTree 
                    (Node2 [Leaf2 "my",
                            Node2 [Leaf2 "favorite",
                                    Node2 [Leaf2 "stuff"],
                                    Leaf2 "things",
                                    Leaf2 "ever"],
                            Node2 [Leaf2 "are",
                                    Node2 [Leaf2 "probably",
                                           Leaf2 "cookies"]]])
                ["cookies","probably","stuff"]
                (sort . depthK 3),

            testCase "test_5" $
                assertTree
                    (Node2 [Node2 [Node2 [Leaf2 10]]])
                    []
                    (sort . depthK 2),

            testCase "test_6" $
                assertTree
                    (Node2 [Node2 [Leaf2 1,
                                    Leaf2 2,
                                    Leaf2 3,
                                    Leaf2 4,
                                    Leaf2 5,
                                    Leaf2 6,
                                    Leaf2 7]])
                    [1,2,3,4,5,6,7]
                    (sort . depthK 2),

            testCase "test_7" $
                assertTree
                    (Node2 [Leaf2 1,
                             Leaf2 2,
                             Leaf2 3])
                    [1,2,3]
                    (sort . depthK 1)
        ]
    ]
