{-
"Prog2Test.hs" - Test Cases for HW 2 -> Prog2.hs
West Chester University - CSC 345 - Programming Language Concepts / Paradigms - Fall 2019
Original format provided by: Richard Burns , distributed with permission.
Custom additions for Prog2 by: Mahmoud Gudarzi, Anton Adamovich, and Akash Kumar
AUTHORS GIVE NO GUARANTEES THAT TEST CASES ARE CORRECT OR COMPLETE.
INTRUCTOR HAS FINAL WORD CONCERNING THE FUNCTIONALITY OF YOUR CODE.
YOU ARE ENCOURAGED TO TEST YOUR CODE INDEPENDENTLY.
Usage: ghci Prog3Test; main
Dependencies: cabal update
              cabal install tasty
              cabal install tasty-hunit
-}
module Prog2Test where
import Prog2

main :: IO ()
main = do
          putStrLn "Testing Prog2: "
          
          putStr "1. threeDifferent: " -- Test wasn't modified from the original test.
          if threeDifferent 1 2 3          == True  && 
             threeDifferent 12 6 5         == True  && 
             threeDifferent (-2) 13 10     == True  && 
             threeDifferent 2 4 (-6)       == True  && 
             threeDifferent 42 42 20       == False && 
             threeDifferent 21 20 21       == False && 
             threeDifferent (-3) (-2) (-3) == False && 
             threeDifferent 3 3 3          == False
          then putStrLn "Works!"
          else putStrLn "Does not work."

          putStr "2. fourDifferent: " -- New test.
          if fourDifferent 1 2 3 4             == True  && 
             fourDifferent 12 6 5 4            == True  && 
             fourDifferent (-2) 13 10 4        == True  && 
             fourDifferent 2 4 (-6) 3          == True  && 
             fourDifferent (-2) (-4) (-6) (-3) == True  &&
             fourDifferent 42 42 20 19         == False &&
             fourDifferent 42 42 42 20         == False &&
             fourDifferent 21 20 21 20         == False && 
             fourDifferent 19 42 20 42         == False &&
             fourDifferent 20 20 42 42         == False &&
             fourDifferent 21 21 21 21         == False && 
             fourDifferent (-3) (-2) (-3) (-1) == False && 
             fourDifferent (-3) (-2) (-3) (-3) == False &&
             fourDifferent (-3) (-3) (-3) (-3) == False && 
             fourDifferent (-3) 1 (-3) (-3)    == False &&
             fourDifferent 3 3 3 3             == False
          then putStrLn "Works!"
          else putStrLn "Does not work."

          putStr "3. sum': "
          if sum' 5 == 15 && 
             sum' 1 == 1 &&
             sum' 0 == 0 &&
             sum' 6 == 21
          then putStrLn "Works!"
          else putStrLn "Does not work."

          putStr "4. asciisum: " -- Functionality changed from abssum.
          if asciisum "Donate $2.70 to Sander's Campaign!" == 2785 && 
             asciisum "#@*(^&#$@"                          == 448  
          then putStrLn "Works!"
          else putStrLn "Does not work."

          putStr "5. integerSqrt: " -- Test wasn't modified from the original test.
          if integerSqrt 9   == 3 &&
             integerSqrt 1   == 1 &&
             integerSqrt 17  == 4 &&
             integerSqrt 102 == 10
          then putStrLn "Works!"
          else putStrLn "Does not work."

          putStr "6. orderTriple: " -- New test.
          if orderTriple (1,2,3)             == (1,2,3)     &&
             orderTriple (22,33,19)          == (19,22,33)  &&
             orderTriple (10,1,(-4))         == ((-4),1,10) &&
             orderTriple (3,3,3)             == (3,3,3)     &&
             orderTriple (3,3,2)             == (2,3,3)     &&
             orderTriple ((-3), (-3), (-2))  == ((-3), (-3), (-2)) &&
             orderTriple ((-42),(-42),(-42)) == ((-42),(-42),(-42))
          then putStrLn "Works!"
          else putStrLn "Does not work."

          putStr "7. swap: " -- Functionality changed
          if swap ('a','b','c','d') == ('d','b','c','a') &&
             swap ('1','2','3','4') == ('4','2','3','1') &&
             swap ('1','1','1','1') == ('1','1','1','1') &&
             swap ('1','2','3','1') == ('1','2','3','1')
          then putStrLn "Works!"
          else putStrLn "Does not work."

          putStr "8. negateTwoDigits: " -- New test.
          if negateTwoDigits [1,2,3,4]                             == [1,2,3,4]                        &&
             negateTwoDigits [12,14,55,33]                         == [(-12),(-14),(-55),(-33)]        &&
             negateTwoDigits [100, 200, 300, 400]                  == [100, 200, 300, 400]             &&
             negateTwoDigits [2,4,(-16),8]                         == [2,4,16,8]                       &&
             negateTwoDigits [(-1),(-2),(-3),(-4)]                 == [(-1),(-2),(-3),(-4)]            &&
             negateTwoDigits [(-12),(-14),(-55),(-33)]             == [12,14,55,33]                    &&
             negateTwoDigits [(-100), (-200), (-300), (-400)]      == [(-100), (-200), (-300), (-400)] &&
             negateTwoDigits [(-2),(-4),16,(-8)]                   == [(-2),(-4),(-16),(-8)]               
          then putStrLn "Works!"
          else putStrLn "Does not work."

          putStr "9. matches: " -- Test wasn't modified from the original test.
          if matches 1 [1,2,3,4]             == [1]         && 
             matches 2 [1,2,2,4]             == [2,2]       && 
             matches 8 [3,2,3,4]             == []          && 
             matches 42 []                   == []          && 
             matches 12 [12]                 == [12]        && 
             matches 3 [3,2,3,3,2,3,2,2,2,3] == [3,3,3,3,3] && 
             matches (-2) [1,2,4,(-2),(-2),7,88,(-1),(-2),(-19)] 
                                             == [(-2),(-2),(-2)]
          then putStrLn "Works!"
          else putStrLn "Does not work."

          putStr "10. element: " -- Test wasn't modified from the original test.
          if element 1 [1,2,3]       == True  &&
             element 6 [3,6,9]       == True  &&
             element 2 [3,2,1,2]     == True  &&
             element (-10) [(-10),2] == True  &&
             element 4 [1,2,3]       == False &&
             element (-7) [1,7,-4]   == False &&
             element 3 []            == False
          then putStrLn "Works!"
          else putStrLn "Does not work."