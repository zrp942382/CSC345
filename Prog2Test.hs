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

import Prog2

main :: IO ()
main = do
          putStrLn "Testing Prog2: "

          putStr "1. threeDifferent: "
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

          putStr "2. sum': "
          if sum' 1     == sum [1]          &&
             sum' 2     == sum [1,2]        &&
             sum' 10    == sum [1 .. 10]    &&
             sum' 42    == sum [1 .. 42]    &&
             sum' 1000  == sum [1 .. 1000]  &&
             sum' 99999 == sum [1 .. 99999]

          then putStrLn "Works!"
          else putStrLn "Does not work."

          putStr "3. abssum: "
          if abssum 1 95        == sum [1 .. 95] && 
             abssum (-1) 5      == 16            && 
             abssum (-8) (-5)   == 26            && 
             abssum 0 0         == 0             && 
             abssum 1 1         == 1             && 
             abssum (-13) (-13) == 13
          then putStrLn "Works!"
          else putStrLn "Does not work."

          putStr "4. integerSqrt: "
          if integerSqrt 9   == 3 &&
             integerSqrt 1   == 1 &&
             integerSqrt 17  == 4 &&
             integerSqrt 102 == 10
          then putStrLn "Works!"
          else putStrLn "Does not work."

          putStr "5. exponent': "
          if exponent' 0 0    == 1       &&
             exponent' 1 0    == 1       &&
             exponent' 1 1000 == 1       &&
             exponent' 2 5    == 32      &&
             exponent' 10 6   == 1000000 &&
             exponent' (-4) 5 == (-1024)
          then putStrLn "Works!"
          else putStrLn "Does not work."

          putStr "6. largeSmall: "
          if largeSmall (1,2,3)             == (3,1)     &&
             largeSmall (22,33,19)          == (33,19)   &&
             largeSmall (10,1,-4)           == (10,(-4)) &&
             largeSmall (3,3,3)             == (3,3)     &&
             largeSmall ((-42),(-42),(-42)) == ((-42),(-42))
          then putStrLn "Works!"
          else putStrLn "Does not work."

          putStr "7. swap: "
          if swap ('a','b','c','d') == ('a','c','b','d') &&
             swap ('1','2','3','4') == ('1','3','2','4')
          then putStrLn "Works!"
          else putStrLn "Does not work."

          putStr "8. negateOdds: "
          if negateOdds [1,2,3,4]        == [(-1),2,(-3),4]     &&
             negateOdds [12,14,55,33]    == [12,14,(-55),(-33)] &&
             negateOdds [(-3),5,7,(-17)] == [3,(-5),(-7),17]    &&
             negateOdds [2,4,(-6),8]     == [2,4,(-6),8]
          then putStrLn "Works!"
          else putStrLn "Does not work."

          putStr "9. matches: "
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

          putStr "10. element: "
          if element 1 [1,2,3]       == True  &&
             element 6 [3,6,9]       == True  &&
             element 2 [3,2,1,2]     == True  &&
             element (-10) [(-10),2] == True  &&
             element 4 [1,2,3]       == False &&
             element (-7) [1,7,-4]   == False &&
             element 3 []            == False
          then putStrLn "Works!"
          else putStrLn "Does not work."
