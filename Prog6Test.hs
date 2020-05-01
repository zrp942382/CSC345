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

-- From: https://smthngsmwhr.wordpress.com/2012/11/09/sorting-algorithms-in-haskell/
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [y | y <- xs, y <= x] ++ [x] ++ quicksort [y | y <- xs, y > x]

setToList :: Set -> [Int]
setToList (Set x) = [i | i <- x]

maybeSetToList :: Maybe Set -> [Int]
maybeSetToList (Just (Set x)) = [i | i <- x]

main :: IO ()
main = do
          putStrLn "Testing Prog6: "
          
          putStr "1. equal: " 
          if   equal (Set []) (Set []) == True &&
               equal EmptySet EmptySet == True && 
               equal (Set []) EmptySet == True &&
               equal EmptySet (Set []) == True &&
               equal (Set [1]) (Set []) == False &&
               equal (Set [1]) EmptySet == False &&
               equal (Set []) (Set [1]) == False &&
               equal EmptySet (Set [1]) == False &&
               equal (Set [1]) (Set [1]) == True &&
               equal (Set [1,2]) (Set [2,1]) == True &&
               equal (Set [1,2,3,4,5]) (Set [4,1,3,2,5]) == True &&
               equal (Set [1,2,3,4]) (Set [1,2,3,4,5]) == False &&
               equal (Set [1,2,3,4,5]) (Set [1,2,3,4]) == False
          then putStrLn "Works!"
          else putStrLn "Does not work."

          putStr "2. saferemove: "  -- 
          if    maybeSetToList ((saferemove 1 (Set [1,2]))) == [2] &&
                maybeSetToList ((saferemove 1 (Set [2,1]))) == [2] &&
                quicksort (maybeSetToList ((saferemove 1 (Set [1,3,2,4,5])))) == [2,3,4,5] &&
                quicksort (maybeSetToList ((saferemove 1 (Set [2,3,1,4,5])))) == [2,3,4,5] &&
                quicksort (maybeSetToList ((saferemove 1 (Set [2,3,5,4,1])))) == [2,3,4,5] 
                -- maybeSetToList ((saferemove 1 (Set [1]))) == EmptySet
          then putStrLn "Works!"
          else putStrLn "Does not work."

          putStr "3. union: "
          if    setToList (union (Set [1]) (Set [1])) == [1] &&
                quicksort (setToList (union (Set [1,2,3,4]) (Set [3,4,5,6,7]))) == [1,2,3,4,5,6,7] &&
                quicksort (setToList (union (Set [3,4,5,6,7]) (Set [1,2,3,4]))) == [1,2,3,4,5,6,7] &&
                quicksort (setToList (union (Set [1,2,3]) (Set [4,5,6]))) == [1,2,3,4,5,6] &&
                setToList (union (Set []) (Set [1])) == [1] &&
                setToList (union EmptySet (Set [1])) == [1] &&
                setToList (union (Set [1]) (Set [])) == [1] &&
                setToList (union (Set [1]) EmptySet) == [1]
                -- union EmptySet EmptySet == EmptySet
                -- union (Set []) EmptySet == EmptySet
                -- union EmptySet (Set []) == EmptySet
                -- union (Set []) (Set []) == (Set [])
                -- union EmptySet (Set [1]) == (Set [1])
                -- union (Set [1]) EmptySet == (Set [1])
          then putStrLn "Works!"
          else putStrLn "Does not work."

          putStr "4. preorder: " 
          if    preorder (Leaf 0) == [0] &&
                preorder t10 == [0,1,3,4,5,6,7,9,10,11,2] &&
                preorder t5  == [9,13,16,15,17,19,18,20,8,2,1,3,5,4,6] &&
                preorder t5' == [9,13,16,15,17,19,18,20,8,2,1,3,22] &&
                preorder t5'' == [9,13,16,15,17,19,18,20,8] &&
                preorder tree4 == [0,0,0,5,0,0,0,3,1] &&
                preorder tr2' == [0,3,0] &&
                preorder tr2''' == [0,33,1,19,18,20,0,3,0] 
          then putStrLn "Works!"
          else putStrLn "Does not work."

          putStr "5. postorder: " 
          if    postorder (Leaf 0) == [0] &&
                postorder t10 == [3,5,9,10,7,11,6,4,1,2,0] &&
                postorder t5  == [15,17,16,18,20,19,13,1,3,2,4,6,5,8,9] &&
                postorder t5' == [15,17,16,18,20,19,13,1,3,2,22,8,9] &&
                postorder t5'' == [15,17,16,18,20,19,13,8,9] &&
                postorder tree4 == [0,0,0,3,0,5,1,0,0] &&
                postorder tr2' == [3,0,0] &&
                postorder tr2''' == [33,18,20,19,3,0,0,1,0] 
          then putStrLn "Works!"
          else putStrLn "Does not work."

          putStr "6. countZeros: " 
          if    countZeros (Leaf 0) == 1 &&
                countZeros tr2' == 2 &&
                countZeros t10 == 1 &&
                countZeros t5 == 0 &&
                countZeros t5' == 0 &&
                countZeros t5'' == 0 &&
                countZeros tree4== 6 &&
                countZeros tr2' == 2 &&
                countZeros tr2''' == 3 
          then putStrLn "Works!"
          else putStrLn "Does not work."

          putStr "7. countLeaves: " 
          if   countLeaves tr2''' == 5 
          then putStrLn "Works!"
          else putStrLn "Does not work."

          putStr "8. countInteriorNodes: " 
          if    countInteriorNodes (Leaf 0) == 0 &&
                countInteriorNodes tr2' == 1 &&
                countInteriorNodes t10 == 5 &&
                countInteriorNodes t5 == 7 &&
                countInteriorNodes t5' == 6 &&
                countInteriorNodes t5'' == 4 &&
                countInteriorNodes tree4== 4 &&
                countInteriorNodes tr2' == 1 &&
                countInteriorNodes tr2''' == 4 
          then putStrLn "Works!"
          else putStrLn "Does not work."

          putStr "9. depth: " 
          if    depth (Leaf 0) == 0 &&
                depth tr2' == 1 &&
                depth t10 == 5 &&
                depth t5 == 3 &&
                depth t5' == 3 &&
                depth t5'' == 3 &&
                depth tree4== 4 &&
                depth tr2' == 1 &&
                depth tr2''' == 3 &&
                depth r6 == 3 &&
                depth w15 == 5 &&
                depth r7 == 4 &&
                depth rr5 == 6
          then putStrLn "Works!"
          else putStrLn "Does not work."

          putStr "10. balanced: " 
          if    balanced (Leaf 0) == True &&
                balanced t10 == False &&
                balanced t5 == True &&
                balanced t5' == True &&
                balanced t5'' == False &&
                balanced tree4== False &&
                balanced tr2' == True &&
                balanced tr2''' == False &&
                balanced r6 == True &&
                balanced w15 == True &&
                balanced r7 == False &&
                balanced rr5 == False
          then putStrLn "Works!"
          else putStrLn "Does not work."


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
w0' :: Tree
w0' = Node (Leaf 101) 86 (Leaf 102)
w1 :: Tree
w1 = Node (Leaf 81) 99 (Leaf 80)
w2 :: Tree
w2 = Node (Leaf 83) 98 (Leaf 82)
w3 :: Tree
w3 = Node (Leaf 85) 97 (Leaf 84)
w4 :: Tree
w4 = Node (Leaf 87) 96 (w0')
w5 :: Tree
w5 = Node (Leaf 89) 95 (Leaf 88)
w6 :: Tree
w6 = Node (Leaf 91) 94 (Leaf 90)
w7 :: Tree
w7 = Node (Leaf 11) 7 (Leaf 12)
w8 :: Tree
w8 = Node (Leaf 93) 8 (Leaf 92)
w9 :: Tree
w9 = Node (w1) 9 (w2)
w10 :: Tree
w10 = Node (w3) 10 (w4)
w11 :: Tree
w11 = Node (w5) 5 (w6)
w12 :: Tree
w12 = Node (w7) 6 (w8)
w13 :: Tree
w13 = Node (w9) 1 (w10)
w14 :: Tree
w14 = Node (w11) 2 (w12)
w15 :: Tree
w15 = Node (w13) 0 (w14)
rr :: Tree
rr = Node (Leaf 14) 12 (Leaf 15)
rr1 :: Tree
rr1 = Node (rr) 11 (Leaf 13)
rr2 :: Tree
rr2 = Node (Leaf 10) 8 (rr1)
rr3 :: Tree
rr3 = Node (Leaf 7) 6 (rr2)
rr4 :: Tree
rr4 = Node (rr3) 2 (Leaf 5)
rr5 :: Tree
rr5 = Node (Leaf 1) 0 (rr4)
r1 :: Tree
r1 = Node (Leaf 13) 9 (Leaf 14)
r2 :: Tree 
r2 = Node (Leaf 7) 3 (Leaf 8)
r3 :: Tree
r3 = Node (Leaf 11) 6 (Leaf 12)
r4 :: Tree 
r4 = Node (r1) 5 (Leaf 10)
r5 :: Tree
r5 = Node (r2) 1 (Leaf 4)
r6 :: Tree
r6 = Node (r4) 2 (r3)
r7 :: Tree
r7 = Node (r5) 0 (r6)
