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
          if fourDifferent 1 2 3 4              == True  && 
             fourDifferent 12 6 5 4             == True  && 
             fourDifferent (-2) 13 10 4         == True  && 
             fourDifferent 2 4 (-6) 3           == True  && 
             fourDifferent (-2) (-4) (-6) (-3)  == True  &&
             fourDifferent 42 42 20 19          == False &&
             fourDifferent 42 42 42 20          == False &&
             fourDifferent 21 20 21 20          == False && 
             fourDifferent 19 42 20 42          == False &&
             fourDifferent 20 20 42 42          == False &&
             fourDifferent 21 21 21 21          == False && 
             fourDifferent (-10) (-10) 99 (-99) == False &&
             fourDifferent 10 (-10) (-99) 99    == True  &&
             fourDifferent 10 (-99) (-10) (-99) == False &&
             fourDifferent 10 99 99 (-10)       == False &&
             fourDifferent 99 10 (-10) 99       == False &&
             fourDifferent 99 10 (-99) 10       == False &&
             fourDifferent 99 (-10) (-10) (-99) == False &&
             fourDifferent 99 (-10) (-99) (-10) == False &&
             fourDifferent (-3) (-2) (-3) (-1) == False  && 
             fourDifferent (-3) (-2) (-3) (-3) == False  &&
             fourDifferent (-3) (-3) (-3) (-3) == False  && 
             fourDifferent (-3) 1 (-3) (-3)    == False  &&
             fourDifferent 3 3 3 3             == False
          then putStrLn "Works!"
          else putStrLn "Does not work."

          putStr "3. sum': " -- Test wasn't modified from original test.
          if sum' 1     == sum [1]          &&
             sum' 2     == sum [1,2]        &&
             sum' 10    == sum [1 .. 10]    &&
             sum' 42    == sum [1 .. 42]    &&
             sum' 1000  == sum [1 .. 1000]  &&
             sum' 99999 == sum [1 .. 99999]
          then putStrLn "Works!"
          else putStrLn "Does not work."

          putStr "4. asciisum: " -- Functionality changed from abssum.
          if asciisum "Donate $2.70 to Sander's Campaign!" == 2785 && 
             asciisum "#@*(^&#$@"                          == 448  &&
             asciisum " "                                  == 32   &&
             asciisum "  "                                 == 64   &&
             asciisum " !"                                 == 65   &&
             asciisum ""                                   == 0    &&
             asciisum "(2<"                                == 150  
          then putStrLn "Works!"
          else putStrLn "Does not work."

          putStr "5. integerSqrt: " -- Test wasn't modified from the original test.
          if integerSqrt 9   == 3 &&
             integerSqrt 1   == 1 &&
             integerSqrt 0   == 0 &&
             integerSqrt 10  == 3 &&
             integerSqrt 15  == 3 &&
             integerSqrt 16  == 4 &&
             integerSqrt 17  == 4 &&
             integerSqrt 27  == 5 &&
             integerSqrt 102 == 10
          then putStrLn "Works!"
          else putStrLn "Does not work."

          putStr "6. orderTriple: " -- New test.
          if orderTriple (1,2,3)             == (1,2,3)     &&
             orderTriple (1,3,2)             == (1,2,3)     &&
             orderTriple (2,1,3)             == (1,2,3)     &&
             orderTriple (2,3,1)             == (1,2,3)     &&
             orderTriple (3,1,2)             == (1,2,3)     &&
             orderTriple (3,2,1)             == (1,2,3)     &&
             orderTriple (-1,-2,-3)          == (-3,-2,-1)  &&
             orderTriple (-1,-3,-2)          == (-3,-2,-1)  &&
             orderTriple (-2,-1,-3)          == (-3,-2,-1)  &&
             orderTriple (-2,-3,-1)          == (-3,-2,-1)  &&
             orderTriple (-3,-1,-2)          == (-3,-2,-1)  &&
             orderTriple (-3,-2,-1)          == (-3,-2,-1)  &&
             orderTriple (2,2,1)             == (1,2,2)     &&
             orderTriple (2,1,2)             == (1,2,2)     &&
             orderTriple (1,2,2)             == (1,2,2)     &&
             orderTriple (-2,-2,-1)          == (-2,-2,-1)  &&
             orderTriple (-2,-1,-2)          == (-2,-2,-1)  &&
             orderTriple (-1,-2,-2)          == (-2,-2,-1)  &&
             orderTriple (1,-1,0)            == (-1,0,1)    &&
             orderTriple (22,33,19)          == (19,22,33)  &&
             orderTriple (10,1,(-4))         == ((-4),1,10) &&
             orderTriple (10,(-4),1)         == ((-4),1,10) &&
             orderTriple ((-4),1,10)         == ((-4),1,10) &&
             orderTriple (3,3,3)             == (3,3,3)     &&
             orderTriple (-3,-3,-3)          == (-3,-3,-3)  &&
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
          if negateTwoDigits [1,2,3,4]                             == [1,2,3,4]                     &&
             negateTwoDigits [12,14,55,33]                         == [(-12),(-14),(-55),(-33)]     &&
             negateTwoDigits [100, 200, 300, 400]                  == [100, 200, 300, 400]          &&
             negateTwoDigits [10,-10,99,-99]                       == [-10,10,-99,99]               &&
             negateTwoDigits [10,-10,-99,99]                       == [-10,10,99,-99]               &&
             negateTwoDigits [10,99,-10,-99]                       == [-10,-99,10,99]               &&
             negateTwoDigits [10,99,-99,-10]                       == [-10,-99,99,10]               &&
             negateTwoDigits [99,10,-10,-99]                       == [-99,-10,10,99]               &&
             negateTwoDigits [99,10,-99,-10]                       == [-99,-10,99,10]               &&
             negateTwoDigits [99,-10,10,-99]                       == [-99,10,-10,99]               &&
             negateTwoDigits [99,-10,-99,10]                       == [-99,10,99,-10]               &&
             negateTwoDigits [9,-9,100,-100]                       == [9,-9,100,-100]               &&
             negateTwoDigits [9,-9,-100,100]                       == [9,-9,-100,100]               &&
             negateTwoDigits [9,100,-9,-100]                       == [9,100,-9,-100]               &&
             negateTwoDigits [9,100,-100,-9]                       == [9,100,-100,-9]               &&
             negateTwoDigits [100,9,-9,-100]                       == [100,9,-9,-100]               &&
             negateTwoDigits [100,9,-100,-9]                       == [100,9,-100,-9]               &&
             negateTwoDigits [100,-9,9,-100]                       == [100,-9,9,-100]               &&
             negateTwoDigits [100,-9,-100,9]                       == [100,-9,-100,9]               &&
             negateTwoDigits [2,4,(-16),8]                         == [2,4,16,8]                    &&
             negateTwoDigits [(-1),(-2),(-3),(-4)]                 == [(-1),(-2),(-3),(-4)]         &&
             negateTwoDigits [(-12),(-14),(-55),(-33)]             == [12,14,55,33]                 &&
             negateTwoDigits [(-100), (-200), (-300), (-400)]      == [(-100),(-200),(-300),(-400)] &&
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
             matches (-2) [1,2,4,(-2),(-2),7,88,(-1),(-2),(-19)] == [(-2),(-2),(-2)]
          then putStrLn "Works!"
          else putStrLn "Does not work."

          putStr "10. element: " -- Modified test, more cases.
          if element 1 [1,2,3]       == True                 &&
             element 6 [3,6,9]       == True                 &&
             element 2 [3,2,1,2]     == True                 &&
             element (-10) [(-10),2] == True                 &&
             element 4 [1,2,3]       == False                &&
             element (-7) [1,7,-4]   == False                &&
             element 3 []            == False                &&
             element 0 []            == elem 0 []            &&
             element 3 [2,4,3]       == elem 3 [2,4,3]       &&
             element 5 [2,4,6]       == elem 5 [2,4,6]       &&
             element (-9) [1,2,3]    == elem (-9) [1,2,3]    &&
             element (-9) [-9,2,3]   == elem (-9) [-9,2,3]   &&
             element (-9) [-9,-9,-9] == elem (-9) [-9,-9,-9] &&
             element (-2) [-2,-2,2]  == elem (-2) [-2,-2,2]
          then putStrLn "Works!"
          else putStrLn "Does not work."