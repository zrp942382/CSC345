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
          if    maybeSetToList ((saferemove 1 (Set [1]))) == [] && -- Probably could be EmptySet or (Set []). I'd go with making this equal EmptySet in your own code and not (Set []).
                maybeSetToList ((saferemove 1 (Set [1,2]))) == [2] &&
                maybeSetToList ((saferemove 1 (Set [2,1]))) == [2] &&
                quicksort (maybeSetToList ((saferemove 1 (Set [1,3,2,4,5])))) == [2,3,4,5] &&
                quicksort (maybeSetToList ((saferemove 1 (Set [2,3,1,4,5])))) == [2,3,4,5] &&
                quicksort (maybeSetToList ((saferemove 1 (Set [2,3,5,4,1])))) == [2,3,4,5] 
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
          if    countLeaves (Leaf 0) == 1 &&
                countLeaves tr2' == 2 &&
                countLeaves t10 == 6 &&
                countLeaves t5 == 8 &&
                countLeaves t5' == 7 &&
                countLeaves t5'' == 5 &&
                countLeaves tree4== 5 &&
                countLeaves tr2' == 2 &&
                countLeaves tr2''' == 5 
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