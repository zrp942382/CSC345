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
import Data.List
import System.Environment
import Test.Tasty
import Test.Tasty.HUnit


-- A fn that makes a right skewed tree.
-- makeRightSkewedTree1 :: [a] -> Tree1
-- makeRightSkewedTree1 [] = error "empty tree"
-- makeRightSkewedTree1 (x:[]) = Leaf1 x
-- makeRightSkewedTree1 (x:y:[]) = Node1 x (Leaf1 y) (Leaf1 42)
-- makeRightSkewedTree1 (x:y:xs) = Node1 x (Leaf1 y) (makeRightSkewedTree1 xs)

data Tree3 = Tree3 String [Tree3]

draw :: Tree3 -> [String]
draw (Tree3 x ts0) = x : drawSubTrees ts0
  where
    drawSubTrees [] = []
    drawSubTrees [t] =
        "│" : shift "└ " "   " (draw t)
    drawSubTrees (t:ts) =
        "│" : shift "├ " "│  " (draw t) ++ drawSubTrees ts

    shift first other = zipWith (++) (first : repeat other)

drawTree :: Tree3 -> String
drawTree = unlines . draw

drawTree1 :: Tree1 -> String
drawTree1 = drawTree . tree1as3
  where
    tree1as3 :: Tree1 -> Tree3
    tree1as3 (Leaf1 x)     = Tree3 (show x) []
    tree1as3 (Node1 x l r) = Tree3 (show x) [tree1as3 l, tree1as3 r]

drawTree2 :: Show a => Tree2 a -> String
drawTree2 t = drawTree (tree2as3 t "[root]")
  where
    tree2as3 :: Show a => Tree2 a -> String -> Tree3
    tree2as3 (Leaf2 x) _       = Tree3 (show x) []
    tree2as3 (Node2 ns) spacer = Tree3 spacer [tree2as3 n "─┐" | n <- ns]

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
            testCase "test_1" $
              assertEqual
                ("input tree: \n" <> drawTree1 (Leaf1 1))
                [1]
                (preorder (Leaf1 1)),

            testCase "test_2" $
              assertEqual
                ("input tree: \n" <> drawTree1 (Node1 1
                                                      (Leaf1 2)
                                                      (Leaf1 3)))
                [1,2,3]
                (preorder (Node1 1
                                 (Leaf1 2)
                                 (Leaf1 3))),
            testCase "test_3" $
              assertEqual
                ("input tree: \n" <> drawTree1 (Node1 1
                                                      (Node1 2 (Leaf1 3)
                                                               (Leaf1 4))
                                                      (Node1 5 (Leaf1 6)
                                                               (Leaf1 7))))
                [1,2,3,4,5,6,7]
                (preorder (Node1 1
                                 (Node1 2
                                        (Leaf1 3)
                                        (Leaf1 4))
                                 (Node1 5
                                        (Leaf1 6)
                                        (Leaf1 7)))),
            testCase "test_4" $
              assertEqual
                ("input tree: \n" <> drawTree1 (Node1 1
                                                      (Node1 2
                                                             (Node1 3
                                                                    (Leaf1 4)
                                                                    (Leaf1 5))
                                                             (Leaf1 6))
                                                      (Node1 7
                                                             (Leaf1 8)
                                                             (Leaf1 9))))
                [1,2,3,4,5,6,7,8,9]
                (preorder (Node1 1
                                 (Node1 2
                                        (Node1 3
                                               (Leaf1 4)
                                               (Leaf1 5))
                                        (Leaf1 6))
                                 (Node1 7
                                        (Leaf1 8)
                                        (Leaf1 9))))
        ],

        -- 2: postorder :: Tree1 -> [Int]
        testGroup "test_postorder" [
            testCase "test_1" $
              assertEqual
                ("input tree: \n" <> drawTree1 (Leaf1 1))
                [1]
                (postorder (Leaf1 1)),

            testCase "test_2" $
              assertEqual
                ("input tree: \n" <> drawTree1 (Node1 1
                                                      (Leaf1 2)
                                                      (Leaf1 3)))
                [1,2,3]
                (postorder (Node1 3
                                  (Leaf1 1)
                                  (Leaf1 2))),

            testCase "test_3" $
              assertEqual
                ("input tree: \n" <> drawTree1 (Node1 1
                                                      (Node1 2 (Leaf1 3)
                                                               (Leaf1 4))
                                                      (Node1 5 (Leaf1 6)
                                                               (Leaf1 7))))
                [1,2,3,4,5,6,7]
                (postorder (Node1 7
                                  (Node1 3
                                         (Leaf1 1)
                                         (Leaf1 2))
                                  (Node1 6
                                         (Leaf1 4)
                                         (Leaf1 5)))),

            testCase "test_4" $
              assertEqual
                ("input tree: \n" <> drawTree1 (Node1 9
                                                      (Node1 5
                                                             (Node1 3
                                                                    (Leaf1 1)
                                                                    (Leaf1 2))
                                                             (Leaf1 4))
                                                      (Node1 8
                                                             (Leaf1 6)
                                                             (Leaf1 7))))
                [1,2,3,4,5,6,7,8,9]
                (postorder (Node1 9
                                  (Node1 5
                                         (Node1 3
                                                (Leaf1 1)
                                                (Leaf1 2))
                                         (Leaf1 4))
                                  (Node1 8
                                         (Leaf1 6)
                                         (Leaf1 7))))
        ],

        -- 3: sumPositives :: Tree1 -> Int
        testGroup "test_sumPositives" [
            testCase "test_1" $
              assertEqual
                ("input tree: \n" <> drawTree1 (Leaf1 1))
                1
                (sumPositives (Leaf1 1)),
            testCase "test_2" $
              assertEqual
                ("input tree: \n" <> drawTree1 (Leaf1 (-3)))
                0
                (sumPositives (Leaf1 (-3))),
            testCase "test_3" $
              assertEqual
                ("input tree: \n" <> drawTree1 (Node1 3
                                                      (Leaf1 1)
                                                      (Leaf1 2)))
                6
                (sumPositives (Node1 3
                                     (Leaf1 1)
                                     (Leaf1 2))),
            testCase "test_4" $
              assertEqual
                ("input tree: \n" <> drawTree1 (Node1 (-5)
                                                      (Leaf1 3)
                                                      (Leaf1 4)))
                7
                (sumPositives (Node1 (-5)
                                     (Leaf1 3)
                                     (Leaf1 4))),
            testCase "test_5" $
              assertEqual
                ("input tree: \n" <> drawTree1 (Node1 (-5)
                                                      (Leaf1 (-9))
                                                      (Leaf1 100)))
                100
                (sumPositives (Node1 (-5)
                                     (Leaf1 (-9))
                                     (Leaf1 100))),
            testCase "test_6" $
              assertEqual
                ("input tree: \n" <> drawTree1 (Node1 7
                                                      (Node1 3
                                                             (Leaf1 1)
                                                             (Leaf1 2))
                                                      (Node1 6
                                                             (Leaf1 4)
                                                             (Leaf1 5))))
                28
                (sumPositives (Node1 7
                                     (Node1 3
                                            (Leaf1 1)
                                            (Leaf1 2))
                                     (Node1 6
                                            (Leaf1 4)
                                            (Leaf1 5)))),
            testCase "test_7" $
              assertEqual
                ("input tree: \n" <> drawTree1 (Node1 9
                                                      (Node1 (-5)
                                                             (Node1 3
                                                                    (Leaf1 1)
                                                                    (Leaf1 2))
                                                             (Leaf1 (-1)))
                                                      (Node1 8
                                                             (Leaf1 6)
                                                             (Leaf1 (-2)))))
                29
                (sumPositives (Node1 9
                                     (Node1 (-5)
                                            (Node1 3
                                                   (Leaf1 1)
                                                   (Leaf1 2))
                                            (Leaf1 (-1)))
                                     (Node1 8
                                            (Leaf1 6)
                                            (Leaf1 (-2)))))
        ],

        -- 4: countLeaves :: Tree1 -> Int
        testGroup "test_countLeaves" [
            testCase "test_1" $
              assertEqual
                ("input tree: \n" <> drawTree1 (Leaf1 1))
                1
                (countLeaves (Leaf1 1)),

            testCase "test_2" $
              assertEqual
                ("input tree: \n" <> drawTree1 (Node1 3
                                                      (Leaf1 1)
                                                      (Leaf1 2)))
                2
                (countLeaves (Node1 3
                                    (Leaf1 1)
                                    (Leaf1 2))),
            testCase "test_3" $
              assertEqual
                ("input tree: \n" <> drawTree1 (Node1 7
                                                      (Node1 3
                                                             (Leaf1 1)
                                                             (Leaf1 2))
                                                      (Node1 6
                                                             (Leaf1 4)
                                                             (Leaf1 5))))
                4
                (countLeaves (Node1 7
                                    (Node1 3
                                           (Leaf1 1)
                                           (Leaf1 2))
                                    (Node1 6
                                           (Leaf1 4)
                                           (Leaf1 5)))),
            testCase "test_4" $
              assertEqual
                ("input tree: \n" <> drawTree1 (Node1 9
                                                      (Node1 5
                                                             (Node1 3
                                                                    (Leaf1 1)
                                                                    (Leaf1 2))
                                                             (Leaf1 4))
                                                      (Node1 8
                                                             (Leaf1 6)
                                                             (Leaf1 7))))
                5
                (countLeaves (Node1 9
                                    (Node1 5
                                           (Node1 3
                                                  (Leaf1 1)
                                                  (Leaf1 2))
                                           (Leaf1 4))
                                    (Node1 8
                                           (Leaf1 6)
                                           (Leaf1 7))))
        ],

        -- 5: depth :: Tree1 -> Int
        testGroup "test_depth" [
            testCase "test_1" $
              assertEqual
                ("input tree: \n" <> drawTree1 (Leaf1 1))
                0
                (depth (Leaf1 1)),
            testCase "test_2" $
              assertEqual
                ("input tree: \n" <> drawTree1 (Node1 3
                                                      (Leaf1 1)
                                                      (Leaf1 2)))
                1
                (depth (Node1 3
                              (Leaf1 1)
                              (Leaf1 2))),
            testCase "test_3" $
              assertEqual
                ("input tree: \n" <> drawTree1 (Node1 7
                                                      (Node1 3
                                                             (Leaf1 1)
                                                             (Leaf1 2))
                                                      (Node1 6 (Leaf1 4)
                                                               (Leaf1 5))))
                2
                (depth (Node1 7
                              (Node1 3
                                     (Leaf1 1)
                                     (Leaf1 2))
                              (Node1 6
                                     (Leaf1 4)
                                     (Leaf1 5)))),
            testCase "test_4" $
              assertEqual
                ("input tree: \n" <> drawTree1 (Node1 9
                                                      (Node1 5
                                                             (Node1 3
                                                                    (Leaf1 1)
                                                                    (Leaf1 2))
                                                             (Leaf1 4))
                                                      (Node1 8
                                                             (Leaf1 6)
                                                             (Leaf1 7))))
                3
                (depth (Node1 9
                              (Node1 5
                                     (Node1 3
                                            (Leaf1 1)
                                            (Leaf1 2))
                                     (Leaf1 4))
                              (Node1 8
                                     (Leaf1 6)
                                     (Leaf1 7))))
        ],

        -- 6: occurs :: Eq a => a -> Tree2 a -> Bool
        testGroup "test_occurs" [
            testCase "test_1" $
              assertEqual
                ("input tree: \n" <> drawTree2 (Leaf2 10))
                False
                (occurs 9 (Leaf2 10)),

            testCase "test_2" $
              assertEqual
                ("input tree: \n" <> drawTree2 (Node2 [Leaf2 'b',
                                                       Node2 [Leaf2 'z',
                                                              Leaf2 'a'],
                                                       Node2 [Leaf2 'c',
                                                              Node2 [Leaf2 'd',
                                                                     Leaf2 'f']]]))
                True
                (occurs 'a' (Node2 [Leaf2 'b',
                                    Node2 [Leaf2 'z',
                                           Leaf2 'a'],
                                    Node2 [Leaf2 'c',
                                           Node2 [Leaf2 'd',
                                                  Leaf2 'f']]])),

            testCase "test_3" $
              assertEqual
                ("input tree: \n" <> drawTree2 (Node2 [Leaf2 'b',
                                                       Node2 [Leaf2 'z',
                                                              Leaf2 'a'],
                                                       Node2 [Leaf2 'c',
                                                              Node2 [Leaf2 'd',
                                                                     Leaf2 'f']]]))
                False
                (occurs 'e' (Node2 [Leaf2 'b',
                                    Node2 [Leaf2 'z',
                                           Leaf2 'a'],
                                    Node2 [Leaf2 'c',
                                           Node2 [Leaf2 'd',
                                                  Leaf2 'f']]])),

            testCase "test_4" $
              assertEqual
                ("input tree: \n" <> drawTree2 (Node2 [Leaf2 "my",
                                                       Node2 [Leaf2 "favorite",
                                                              Node2 [Leaf2 "stuff"],
                                                              Leaf2 "things",
                                                              Leaf2 "ever"],
                                                       Node2 [Leaf2 "are",
                                                              Node2 [Leaf2 "probably",
                                                                     Leaf2 "cookies"]]]))
                False
                (occurs "cool" (Node2 [Leaf2 "my",
                                       Node2 [Leaf2 "favorite",
                                              Node2 [Leaf2 "stuff"],
                                              Leaf2 "things",
                                              Leaf2 "ever"],
                                       Node2 [Leaf2 "are",
                                              Node2 [Leaf2 "probably",
                                                     Leaf2 "cookies"]]])),

            testCase "test_5" $
              assertEqual
                ("input tree: \n" <> drawTree2 (Node2 [Node2 [Node2 [Leaf2 10]]]))
                True
                (occurs 10 (Node2 [Node2 [Node2 [Leaf2 10]]])),

            testCase "test_6" $
              assertEqual
                ("input tree: \n" <> drawTree2 (Node2 [Node2
                                                       [Leaf2 100000,
                                                        Leaf2 200001,
                                                        Leaf2 200002,
                                                        Leaf2 200003,
                                                        Leaf2 200004,
                                                        Leaf2 200000,
                                                        Leaf2 500000]]))
                True
                (occurs 500000 (Node2 [Node2 [Leaf2 100000,
                                              Leaf2 200001,
                                              Leaf2 200002,
                                              Leaf2 200003,
                                              Leaf2 200004,
                                              Leaf2 200000,
                                              Leaf2 500000]])),

            testCase "test_7" $
              assertEqual
                ("input tree: \n" <> drawTree2 (Node2 [Leaf2 False,
                                                       Leaf2 False,
                                                       Leaf2 False]))
                False
                (occurs True (Node2 [Leaf2 False,
                                     Leaf2 False,
                                     Leaf2 False]))
        ],

        -- 7: countInteriorNodes :: Tree2 a -> Int
        testGroup "test_countInteriorNodes" [
            testCase "test_1" $
              assertEqual
                ("input tree: \n" <> drawTree2 (Leaf2 10))
                1
                (countInteriorNodes (Leaf2 10)),

            testCase "test_2" $
              assertEqual
                ("input tree: \n" <> drawTree2 (Node2 [Leaf2 'b',
                                                       Node2 [Leaf2 'z',
                                                              Leaf2 'a'],
                                                       Node2 [Leaf2 'c',
                                                              Node2 [Leaf2 'd',
                                                                     Leaf2 'f']]]))
                6
                (countInteriorNodes (Node2 [Leaf2 'b',
                                            Node2 [Leaf2 'z',
                                                   Leaf2 'a'],
                                            Node2 [Leaf2 'c',
                                                   Node2 [Leaf2 'd',
                                                          Leaf2 'f']]])),

            testCase "test_3" $
              assertEqual
                ("input tree: \n" <> drawTree2 (Node2 [Leaf2 'b',
                                                       Node2 [Leaf2 'z',
                                                              Leaf2 'a'],
                                                       Node2 [Leaf2 'c',
                                                              Node2 [Leaf2 'd',
                                                                     Leaf2 'f']]]))
                6
                (countInteriorNodes (Node2 [Leaf2 'b',
                                            Node2 [Leaf2 'z',
                                                   Leaf2 'a'],
                                            Node2 [Leaf2 'c',
                                                   Node2 [Leaf2 'd',
                                                          Leaf2 'f']]])),

            testCase "test_4" $
              assertEqual
                ("input tree: \n" <> drawTree2 (Node2 [Leaf2 "my",
                                                       Node2 [Leaf2 "favorite",
                                                              Node2 [Leaf2 "stuff"],
                                                              Leaf2 "things",
                                                              Leaf2 "ever"],
                                                       Node2 [Leaf2 "are",
                                                              Node2 [Leaf2 "probably",
                                                                     Leaf2 "cookies"]]]))
                8
                (countInteriorNodes (Node2 [Leaf2 "my",
                                            Node2 [Leaf2 "favorite",
                                                   Node2 [Leaf2 "stuff"],
                                                   Leaf2 "things",
                                                   Leaf2 "ever"],
                                            Node2 [Leaf2 "are",
                                                   Node2 [Leaf2 "probably",
                                                          Leaf2 "cookies"]]])),

            testCase "test_5" $
              assertEqual
                ("input tree: \n" <> drawTree2 (Node2 [Node2 [Node2 [Leaf2 10]]]))
                1
                (countInteriorNodes (Node2 [Node2 [Node2 [Leaf2 10]]])),

            testCase "test_6" $
              assertEqual
                ("input tree: \n" <> drawTree2 (Node2 [Node2
                                                       [Leaf2 100000,
                                                              Leaf2 200001,
                                                              Leaf2 200002,
                                                              Leaf2 200003,
                                                              Leaf2 200004,
                                                              Leaf2 200000,
                                                              Leaf2 500000]]))
                7
                (countInteriorNodes (Node2 [Node2 [Leaf2 100000,
                                                   Leaf2 200001,
                                                   Leaf2 200002,
                                                   Leaf2 200003,
                                                   Leaf2 200004,
                                                   Leaf2 200000,
                                                   Leaf2 500000]])),

            testCase "test_7" $
              assertEqual
                ("input tree: \n" <> drawTree2 (Node2 [Leaf2 False,
                                                       Leaf2 False,
                                                       Leaf2 False]))
                3
                (countInteriorNodes (Node2 [Leaf2 False,
                                            Leaf2 False,
                                            Leaf2 False]))

        ],

        -- 8: sumTree :: Tree2 Int -> Int
        testGroup "test_sumTree" [
            testCase "test_1" $
              assertEqual
                ("input tree: \n" <> drawTree2 (Leaf2 10))
                10
                (sumTree (Leaf2 10)),
            testCase "test_2" $
              assertEqual
                ("input tree: \n" <> drawTree2 (Node2 [Leaf2 1,
                                                       Node2 [Leaf2 2,
                                                              Leaf2 3],
                                                       Node2 [Leaf2 4,
                                                              Node2 [Leaf2 5,
                                                                     Leaf2 6]]]))
                21
                (sumTree (Node2 [Leaf2 1,
                                 Node2 [Leaf2 2,
                                        Leaf2 3],
                                 Node2 [Leaf2 4,
                                        Node2 [Leaf2 5,
                                               Leaf2 6]]])),
            testCase "test_3" $
              assertEqual
                ("input tree: \n" <> drawTree2 (Node2 [Leaf2 1,
                                                       Node2 [Leaf2 2,
                                                              Leaf2 3],
                                                       Node2 [Leaf2 4,
                                                              Node2 [Leaf2 5,
                                                                     Leaf2 6]]]))
                21
                (sumTree (Node2 [Leaf2 1,
                                    Node2 [Leaf2 2,
                                           Leaf2 3],
                                    Node2 [Leaf2 4,
                                           Node2 [Leaf2 5,
                                                  Leaf2 6]]])),
            testCase "test_4" $
              assertEqual
                ("input tree: \n" <> drawTree2 (Node2 [Leaf2 1,
                                                       Node2 [Leaf2 2,
                                                              Node2 [Leaf2 3],
                                                              Leaf2 4,
                                                              Leaf2 5],
                                                       Node2 [Leaf2 6,
                                                              Node2 [Leaf2 7,
                                                              Leaf2 8]]]))
                36
                (sumTree (Node2 [Leaf2 1,
                                       Node2 [Leaf2 2,
                                              Node2 [Leaf2 3],
                                              Leaf2 4,
                                              Leaf2 5],
                                       Node2 [Leaf2 6,
                                              Node2 [Leaf2 7,
                                                     Leaf2 8]]])),
            testCase "test_5" $
              assertEqual
                ("input tree: \n" <> drawTree2 (Node2 [Node2 [Node2 [Leaf2 10]]]))
                10
                (sumTree (Node2 [Node2 [Node2 [Leaf2 10]]])),
            testCase "test_6" $
              assertEqual
                ("input tree: \n" <> drawTree2 (Node2 [Node2 [Leaf2 1,
                                                              Leaf2 2,
                                                              Leaf2 3,
                                                              Leaf2 4,
                                                              Leaf2 5,
                                                              Leaf2 6,
                                                              Leaf2 7]]))
                28
                (sumTree (Node2 [Node2 [Leaf2 1,
                                        Leaf2 2,
                                        Leaf2 3,
                                        Leaf2 4,
                                        Leaf2 5,
                                        Leaf2 6,
                                        Leaf2 7]])),
            testCase "test_7" $
              assertEqual
                ("input tree: \n" <> drawTree2 (Node2 [Leaf2 1, Leaf2 2, Leaf2 3]))
                6
                (sumTree (Node2 [Leaf2 1, Leaf2 2, Leaf2 3]))
        ],

        -- 9: pre2 :: Tree2 a -> [a]
        testGroup "test_pre2" [
            testCase "test_1" $
              assertEqual
                ("input tree: \n" <> drawTree2 (Leaf2 10))
                [10]
                (pre2 (Leaf2 10)),
            testCase "test_2" $
              assertEqual
                ("input tree: \n" <> drawTree2 (Node2 [Leaf2 1,
                                                       Node2 [Leaf2 2,
                                                              Leaf2 3],
                                                       Node2 [Leaf2 4,
                                                              Node2 [Leaf2 5,
                                                                     Leaf2 6]]]))
                [1,2,3,4,5,6]
                (pre2 (Node2 [Leaf2 1,
                              Node2 [Leaf2 2,
                                     Leaf2 3],
                              Node2 [Leaf2 4,
                                     Node2 [Leaf2 5,
                                            Leaf2 6]]])),
            testCase "test_3" $
              assertEqual
                ("input tree: \n" <> drawTree2 (Node2 [Leaf2 "my",
                                                       Node2 [Leaf2 "favorite",
                                                              Node2 [Leaf2 "stuff"],
                                                              Leaf2 "things",
                                                              Leaf2 "ever"],
                                                       Node2 [Leaf2 "are",
                                                              Node2 [Leaf2 "probably",
                                                                     Leaf2 "cookies"]]]))
                ["my","favorite","stuff","things","ever","are","probably","cookies"]
                (pre2 (Node2 [Leaf2 "my",
                              Node2 [Leaf2 "favorite",
                                     Node2 [Leaf2 "stuff"],
                                     Leaf2 "things",
                                     Leaf2 "ever"],
                              Node2 [Leaf2 "are",
                                     Node2 [Leaf2 "probably",
                                            Leaf2 "cookies"]]])),
            testCase "test_4" $
              assertEqual
                ("input tree: \n" <> drawTree2 (Node2 [Node2 [Node2 [Leaf2 10]]]))
                [10]
                (pre2 (Node2 [Node2 [Node2 [Leaf2 10]]])),
            testCase "test_5" $
              assertEqual
                ("input tree: \n" <> drawTree2 (Node2 [Node2 [Leaf2 1,
                                                              Leaf2 2,
                                                              Leaf2 3,
                                                              Leaf2 4,
                                                              Leaf2 5,
                                                              Leaf2 6,
                                                              Leaf2 7]]))
                [1,2,3,4,5,6,7]
                (pre2 (Node2 [Node2 [Leaf2 1,
                                     Leaf2 2,
                                     Leaf2 3,
                                     Leaf2 4,
                                     Leaf2 5,
                                     Leaf2 6,
                                     Leaf2 7]])),
            testCase "test_6" $
              assertEqual
                ("input tree: \n" <> drawTree2 (Node2 [Leaf2 1, Leaf2 2, Leaf2 3]))
                [1,2,3]
                (pre2 (Node2 [Leaf2 1, Leaf2 2, Leaf2 3]))
        ],

        -- depthK :: Int -> Tree2 a -> [a]
        testGroup "test_pre2" [
            testCase "test_1" $
              assertEqual
                ("input tree: \n" <> drawTree2 (Leaf2 10))
                [10]
                (depthK 0 (Leaf2 10)),
            testCase "test_2" $
                assertEqual
                  ("input tree: \n" <> drawTree2 (Leaf2 10))
                  []
                  (depthK 1 (Leaf2 10)),
            testCase "test_3" $
              assertEqual
                ("input tree: \n" <> drawTree2 (Node2 [Leaf2 1,
                                                       Node2 [Leaf2 2,
                                                              Leaf2 3],
                                                       Node2 [Leaf2 4,
                                                              Node2 [Leaf2 5,
                                                                     Leaf2 6]]]))
                [2,3,4]
                (sort (depthK 2 (Node2 [Leaf2 1,
                                        Node2 [Leaf2 2,
                                               Leaf2 3],
                                        Node2 [Leaf2 4,
                                               Node2 [Leaf2 5,
                                                      Leaf2 6]]]))),
            testCase "test_4" $
              assertEqual
                ("input tree: \n" <> drawTree2 (Node2 [Leaf2 "my",
                                                       Node2 [Leaf2 "favorite",
                                                              Node2 [Leaf2 "stuff"],
                                                              Leaf2 "things",
                                                              Leaf2 "ever"],
                                                       Node2 [Leaf2 "are",
                                                              Node2 [Leaf2 "probably",
                                                                     Leaf2 "cookies"]]]))
                ["cookies","probably","stuff"]
                (sort (depthK 3 (Node2 [Leaf2 "my",
                                        Node2 [Leaf2 "favorite",
                                               Node2 [Leaf2 "stuff"],
                                               Leaf2 "things",
                                               Leaf2 "ever"],
                                        Node2 [Leaf2 "are",
                                               Node2 [Leaf2 "probably",
                                                      Leaf2 "cookies"]]]))),
            testCase "test_5" $
              assertEqual
                ("input tree: \n" <> drawTree2 (Node2 [Node2 [Node2 [Leaf2 10]]]))
                []
                (depthK 2 (Node2 [Node2 [Node2 [Leaf2 10]]])),
            testCase "test_6" $
              assertEqual
                ("input tree: \n" <> drawTree2 (Node2 [Node2 [Leaf2 1,
                                                              Leaf2 2,
                                                              Leaf2 3,
                                                              Leaf2 4,
                                                              Leaf2 5,
                                                              Leaf2 6,
                                                              Leaf2 7]]))
                [1,2,3,4,5,6,7]
                (sort (depthK 2 (Node2 [Node2 [Leaf2 1,
                                               Leaf2 2,
                                               Leaf2 3,
                                               Leaf2 4,
                                               Leaf2 5,
                                               Leaf2 6,
                                               Leaf2 7]]))),
            testCase "test_7" $
              assertEqual
                ("input tree: \n" <> drawTree2 (Node2 [Leaf2 1, Leaf2 2, Leaf2 3]))
                [1,2,3]
                (sort (depthK 1 (Node2 [Leaf2 1, Leaf2 2, Leaf2 3])))
        ]
    ]
