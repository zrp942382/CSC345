 {-
"Prog4Test.hs" - Test Cases for HW 4 -> Prog4.hs
West Chester University - CSC 345 - Programming Language Concepts / Paradigms - Fall 2019
Original format provided by: Richard Burns , distributed with permission.
Custom additions for Prog4 by: Mahmoud Gudarzi, Anton Adamovich, and Akash Kumar
AUTHORS GIVE NO GUARANTEES THAT TEST CASES ARE CORRECT OR COMPLETE.
INTRUCTOR HAS FINAL WORD CONCERNING THE FUNCTIONALITY OF YOUR CODE.
YOU ARE ENCOURAGED TO TEST YOUR CODE INDEPENDENTLY.
Usage: ghci Prog4Test; main
Dependencies: cabal update
              cabal install tasty
              cabal install tasty-hunit
-}
module Prog6Test where
import Prog6

main :: IO ()
main = do
          putStrLn "Testing Prog6: "
          
          putStr "1. equal: " 
          if   equal (Set []) (Set []) == True &&
               equal EmptySet EmptySet == True && 
               equal (Set []) EmptySet == True &&
               equal EmptySet (Set []) == True &&
               equal EmptySet (Set [1]) == False &&
               equal (Set []) (Set [1]) == False &&
               equal (Set [1]) (Set [1]) == True &&
               equal (Set [1,2]) (Set [2,1]) == True &&
               equal (Set [1,2,3,4,5]) (Set [4,1,3,2,5]) == True &&
               equal (Set [1,2,3,4]) (Set [1,2,3,4,5]) == False
          then putStrLn "Works!"
          else putStrLn "Does not work."

          putStr "4. preorder: " 
          if    preorder t10 == [0,1,3,4,5,6,7,9,10,11,2] &&
                preorder t5  == [9,13,16,15,17,19,18,20,8,2,1,3,5,4,6] &&
                preorder t5' == [9,13,16,15,17,19,18,20,8,2,1,3,22] &&
                preorder t5'' == [9,13,16,15,17,19,18,20,8] &&
                preorder tree4 == [0,0,0,5,0,0,0,3,1] &&
                preorder tr2' == [0,3,0] &&
                preorder tr2''' == [0,33,1,19,18,20,0,3,0] 
          then putStrLn "Works!"
          else putStrLn "Does not work."

          putStr "5. postorder: " 
          if    postorder t10 == [3,5,9,10,7,11,6,4,1,2,0] &&
                postorder t5  == [15,17,16,18,20,19,13,1,3,2,4,6,5,8,9] &&
                postorder t5' == [15,17,16,18,20,19,13,1,3,2,22,8,9] &&
                postorder t5'' == [15,17,16,18,20,19,13,8,9] &&
                postorder tree4 == [0,0,0,3,0,5,1,0,0] &&
                postorder tr2' == [3,0,0] &&
                postorder tr2''' == [33,18,20,19,3,0,0,1,0] 
          then putStrLn "Works!"
          else putStrLn "Does not work."

          putStr "6. countZeros: " 
          if    countZeros t10 == 1 &&
                countZeros t5 == 0 &&
                countZeros t5' == 0 &&
                countZeros t5'' == 0 &&
                countZeros tree4== 6 &&
                countZeros tr2' == 2 &&
                countZeros tr2''' == 3 
          then putStrLn "Works!"
          else putStrLn "Does not work."

          putStr "7. countLeaves: " 
          if    countLeaves t10 == 6 &&
                countLeaves t5 == 8 &&
                countLeaves t5' == 7 &&
                countLeaves t5'' == 5 &&
                countLeaves tree4== 5 &&
                countLeaves tr2' == 2 &&
                countLeaves tr2''' == 5 
          then putStrLn "Works!"
          else putStrLn "Does not work."

          putStr "8. countInteriorNodes: " 
          if    countInteriorNodes t10 == 5 &&
                countInteriorNodes t5 == 7 &&
                countInteriorNodes t5' == 6 &&
                countInteriorNodes t5'' == 4 &&
                countInteriorNodes tree4== 4 &&
                countInteriorNodes tr2' == 1 &&
                countInteriorNodes tr2''' == 4 
          then putStrLn "Works!"
          else putStrLn "Does not work."

          putStr "9. depth: " 
          if    depth t10 == 5 &&
                depth t5 == 3 &&
                depth t5' == 3 &&
                depth t5'' == 3 &&
                depth tree4== 4 &&
                depth tr2' == 1 &&
                depth tr2''' == 3 
          then putStrLn "Works!"
          else putStrLn "Does not work."

          putStr "10. balanced: " 
          if    balanced t10 == False &&
                balanced t5 == True &&
                balanced t5' == True &&
                balanced t5'' == False &&
                balanced tree4== False &&
                balanced tr2' == True &&
                balanced tr2''' == False 
          then putStrLn "Works!"
          else putStrLn "Does not work."

{-
tr1 :: Tree
tr1 = Node (Leaf 15) 16 (Leaf 17)
tr2 :: Tree
tr2 = Node (Leaf 18) 19 (Leaf 20)
tr2' :: Tree
tr2' = Node (Leaf 3) 0 (Leaf 0)
tr2'' :: Tree 
tr2'' = Node (tr2) 1 (tr2')
tr2''' :: Tree
tr2''' = Node (Leaf 33) 0 (tr2'')
t1 :: Tree
t1 = Node (Leaf 1) 2 (Leaf 3)
t2 :: Tree 
t2 = Node (Leaf 4) 5 (Leaf 6)
t3 :: Tree
t3 = Node (t1) 8 (t2)
t4 :: Tree
t4 = Node (tr1) 13 (tr2)
t5 :: Tree 
t5  = Node (t4) 9 (t3)
t3' :: Tree
t3' = Node (t1) 8 (Leaf 22)
t5' :: Tree
t5' = Node (t4) 9 (t3')
t5'' :: Tree
t5'' = Node (t4) 9 (Leaf 8)
t6 :: Tree
t6 = Node (Leaf 9) 7 (Leaf 10)
t7 :: Tree 
t7 = Node (t6) 6 (Leaf 11)
t8 :: Tree
t8 = Node (Leaf 5) 4 (t7)
t9 :: Tree 
t9 = Node (Leaf 3) 1 (t8)
t10 :: Tree
t10 = Node (t9) 0 (Leaf 2)
tree1 :: Tree
tree1 = Node (Leaf 0) 0 (Leaf 3)
tree2 :: Tree 
tree2 = Node (Leaf 0) 5 (tree1)
tree3 :: Tree
tree3 = Node (tree2) 0 (Leaf 1)
tree4 :: Tree
tree4 = Node (Leaf 0) 0 (tree3)
-}

          

