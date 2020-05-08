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
module Prog7Test where
import Prog7

e1, e2, e3 :: Expr
e1 = Val 5
e2 = Add (Val 3) (Val 2) -- 5
e3 = Sub (Val 6) (Val 2) -- 4
e4 = Mul (Val 3) (Val 2) -- 6
e5 = Div (Val 6) (Val 3) -- 2
e6 = Add (Val 3) (Mul (Val 2) (Val 4)) -- 11
e7 = Add (Val 3) (Mul (Val 8) (Mul (Div (Val 12) (Val 6)) (Val 3))) -- 51
e8 = Add (Sub e5 e4) (Mul (Div e3 e5) (e2)) -- 6
e9 = Sub (Val 3) (Val 3)
e10 = Div (e2) (e9)


double :: Int -> Int
double x = x * 2

main :: IO ()
main = do
          putStrLn "Testing Prog7: "
          
          putStr "1. eval: " 
          if eval e1 == 5 &&
             eval e2 == 5 &&
             eval e3 == 4 &&
             eval e4 == 6 &&
             eval e5 == 2 &&
             eval e6 == 11 &&
             eval e7 == 51 &&
             eval e8 == 6
          then putStrLn "Works!"
          else putStrLn "Does not work."

          putStr "2. maxlit: " 
          if maxlit e1 == 5 &&
             maxlit e2 == 3 &&
             maxlit e3 == 6 &&
             maxlit e4 == 3 &&
             maxlit e5 == 6 &&
             maxlit e6 == 4 &&
             maxlit e7 == 12 &&
             maxlit e8 == 6 
          then putStrLn "Works!"
          else putStrLn "Does not work."

          putStr "3. safeeval: " 
          if safeeval (Div (Val 3) (Val 0)) == Nothing &&
             safeeval e10 == Nothing    
          then putStrLn "Works!"
          else putStrLn "Does not work."

          putStr "4. show: " 
          if show (e1) == "5" &&
             show (e2) == "(3+2)" &&
             show (e3) == "(6-2)" &&
             show (e4) == "(3*2)" &&
             (show (e5) == "(6'div'3)" ||
              show (e5) == "(6/3)") &&
             show (e6) == "(3+(2*4))" &&
             (show (e7) == "(3+(8*((12'div'6)*3)))" ||
              show (e7) == "(3+(8*((12/6)*3)))") &&
             (show (e8) == "(((6'div'3)-(3*2))+(((6-2)'div'(6'div'3))*(3+2)))" ||
              show (e8) == "(((6/3)-(3*2))+(((6-2)/(6/3))*(3+2)))")
          then putStrLn "Works!"
          else putStrLn "Does not work."

          putStr "5. addone: " 
          if show (addone e1) == "6" &&
             show (addone e2) == "(4+3)" &&
             show (addone e3) == "(7-3)" &&
             show (addone e4) == "(4*3)" &&
             (show (addone e5) == "(7'div'4)" || 
              show (addone e5) == "(7/4)") &&
             show (addone e6) == "(4+(3*5))" &&
             (show (addone e7) == "(4+(9*((13'div'7)*4)))" || 
              show (addone e7) == "(4+(9*((13/7)*4)))") &&
             (show (addone e8) == "(((7'div'4)-(4*3))+(((7-3)'div'(7'div'4))*(4+3)))" || 
              show (addone e8) == "(((7/4)-(4*3))+(((7-3)/(7/4))*(4+3)))")
          then putStrLn "Works!"
          else putStrLn "Does not work."

          putStr "6. containing: " 
          if containing [1,4,3,2,5] [5,4,3,2] == False &&
             containing [1,4,3,2,5] [5,4,3,2,1] == True &&
             containing [1,4,3,2,5] [8,5,4,3,2,1,9] == True &&
             containing [] [1] == True &&
             containing [1] [] == False  
          then putStrLn "Works!"
          else putStrLn "Does not work."

          putStr "7. sumSqNeg: " 
          if sumSqNeg [-2, -3, 4] == 13 &&
             sumSqNeg [-2, -3, -4] == 29 &&
             sumSqNeg [2, 3, 4] == 0    
          then putStrLn "Works!"
          else putStrLn "Does not work."

          putStr "8. lengths: " 
          if lengths ["", "a", "aa", "aaa", "aaaa", "aaaaa"] == [0,1,2,3,4,5] &&
             lengths [""] == [0]    
          then putStrLn "Works!"
          else putStrLn "Does not work."

          putStr "9. total: " 
          if total double [1,2,3,4,5] == 30 
          then putStrLn "Works!"
          else putStrLn "Does not work."

          putStr "10. containing' " 
          if containing' [1,4,3,2,5] [5,4,3,2] == False &&
             containing' [1,4,3,2,5] [5,4,3,2,1] == True &&
             containing' [1,4,3,2,5] [8,5,4,3,2,1,9] == True &&
             containing' [] [1] == True &&
             containing' [1] [] == False 
          then putStrLn "Works!"
          else putStrLn "Does not work."
