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
module Prog5Test where
import Prog5

main :: IO ()
main = do
          putStrLn "Testing Prog4: "
          
          putStr "1. reverse': " 
          if   reverse' ([] :: [Int]) == reverse [] &&
               reverse' [1,2,3,4,5] == reverse [1,2,3,4,5] &&
               reverse' "abcdef" == reverse "abcdef" && 
               reverse' [1] == reverse [1]
          then putStrLn "Works!"
          else putStrLn "Does not work."

          putStr "2. isPalindrome: "
          if   isPalindrome "abba" == True &&
               isPalindrome "DabbaD" == True &&
               isPalindrome "a" == True &&
               isPalindrome "abcdef" == False &&
               isPalindrome " a" == False &&
               isPalindrome " a " == True 
          then putStrLn "Works!"
          else putStrLn "Does not work."

          putStr "3. isLonger: "
          if   isLonger (0, 0, 59) (0, 0, 58) == -1 &&
               isLonger (0, 0, 58) (0, 0, 58) == 0 &&
               isLonger (0, 0, 58) (0, 0, 59) == 1 &&
               isLonger (0, 2, 58) (0, 1, 59) == -1 &&
               isLonger (0, 2, 58) (0, 2, 58) == 0 &&
               isLonger (0, 1, 59) (0, 2, 58) == 1 &&
               isLonger (2, 1, 59) (1, 2, 58) == -1 &&
               isLonger (2, 2, 59) (2, 2, 59) == 0 &&
               isLonger (1, 2, 59) (2, 1, 58) == 1
          then putStrLn "Works!"
          else putStrLn "Does not work."

          putStr "4. totalSeconds: "
          if   totalSeconds (01, 01, 01) == 3661 &&
               totalSeconds (1, 00, 00) == 3600 &&
               totalSeconds (0, 01, 1) == 61 &&
               totalSeconds (0, 00, 01) == 1 &&
               totalSeconds (0, 0, 00) == 0 
          then putStrLn "Works!"
          else putStrLn "Does not work."

          putStr "5. isValid: "
          if   isValid (-2, 03, 02) == False &&
               isValid (99, 60, 60) == False &&
               isValid (99, 59, 59) == True &&
               isValid (00, -61, 0) == False &&
               isValid (00, 00, 00) == True &&
               isValid (01, 01, 01) == True
          then putStrLn "Works!"
          else putStrLn "Does not work."

          putStr "6. time2Str: "
          if   time2Str (1, 1, 1) == "01:01:01" &&
               time2Str (05, 01, 01) == "05:01:01" &&
               time2Str (12, 59, 59) == "12:59:59" &&
               time2Str (00, 00, 0) == "00:00:00"
          then putStrLn "Works!"
          else putStrLn "Does not work."

          putStr "7. safeFindBefore: "
          if   safeFindBefore 4 [1,2,3,4,5,6,7] == Just [1,2,3] &&
               safeFindBefore 4 [] == Nothing &&
               safeFindBefore 4 [1,2,3,5,6] == Nothing &&
               safeFindBefore 4 [1,4] == Just [1] &&
               safeFindBefore 4 [4] == Just []
          then putStrLn "Works!"
          else putStrLn "Does not work."

          putStr "8. member: "
          if   member 1 (Set [5,4,3,2,1]) == True &&
               member 1 (Set [1]) == True &&
               member 1 (Set [5,4,3,2,3]) == False &&
               member 1 (Set []) == False
          then putStrLn "Works!"
          else putStrLn "Does not work."

          putStr "9. size: "
          if   size (Set [1,2,3,4,5]) == 5 &&
               size (Set []) == 0 &&
               size (Set [1]) == 1 &&
               size EmptySet == 0 
          then putStrLn "Works!"
          else putStrLn "Does not work."