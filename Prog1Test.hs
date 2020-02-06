{-
"Prog1Test.hs" - Test Cases for HW 1 -> Prog1.hs
West Chester University - CSC 345 - Programming Language Concepts / Paradigms - Fall 2019
Original format provided by: Richard Burns , distributed with permission.
Custom additions for Prog1 by: Mahmoud Gudarzi, Anton Adamovich, and Akash Kumar

AUTHORS GIVE NO GUARANTEES THAT TEST CASES ARE CORRECT OR COMPLETE.
INTRUCTOR HAS FINAL WORD CONCERNING THE FUNCTIONALITY OF YOUR CODE.
YOU ARE ENCOURAGED TO TEST YOUR CODE INDEPENDENTLY.

Usage: ghci Prog3Test; main
Dependencies: cabal update
              cabal install tasty
              cabal install tasty-hunit
-}

import Prog1
import Control.Exception

catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = Control.Exception.catch
main :: IO ()
main = do
          putStrLn "Testing Prog1: "
          putStr "1. isSingleDigit: "
          if isSingleDigit 0 == True &&
             isSingleDigit 1 == True &&
             isSingleDigit 5 == True &&
             isSingleDigit 9 == True &&
             isSingleDigit (-1) == True &&
             isSingleDigit (-2) == True &&
             isSingleDigit (-8) == True &&
             isSingleDigit (10) == False &&
             isSingleDigit (-10) == False &&
             isSingleDigit (-14) == False &&
             isSingleDigit 55 == False
               then putStrLn "Works!"
               else putStrLn "Does not work."
          putStr "2. dividesEvenly: "
          if dividesEvenly 4 2 == True &&
             dividesEvenly (-9) (-3) == True &&
             dividesEvenly (9) 3 == True &&
             dividesEvenly 5 1 == True &&
             dividesEvenly (-9) 6 == False &&
             dividesEvenly 1 3 == False
              then putStrLn "Works!"
              else putStrLn "Does not work."
          putStr "3. middle: "
          if middle 4 2 1 == 2 &&
             middle 2 1 3 == 2 &&
             middle 9 9 9 == 9 &&
             middle 5 1 4 == 4 &&
             middle (-2) 3 2 == 2 &&
             middle 12 18 (-100) == 12 &&
             middle (-1) (-4) (-5) == (-4) &&
             middle (-3) (-2) (-2) == (-2) &&
             middle (-42) (-42) (-42) == (-42)
               then putStrLn "Works!"
               else putStrLn "Does not work."

          putStr "4. nand: "
          if nand False False == True &&
             nand False True == True &&
             nand True False == True &&
             nand True True == False
               then putStrLn "Works!"
               else putStrLn "Does not work."

          putStr "5. triangleArea: "
          if triangleArea 2 4 == 4.0 &&
             triangleArea 2 2 == 2.0 &&
             triangleArea 3 5 == 7.5 &&
             triangleArea (-7) 4 == (-14.0) &&
             triangleArea 3 (-9) == (-13.5) &&
             triangleArea (-10) (-2000) == 10000.0
               then putStrLn "Works!"
               else putStrLn "Does not work."

          putStr "6. floorDecimal: "
          if floorDecimal 9 == 9 &&
             floorDecimal 2.1 == 2 &&
             floorDecimal 3.4 == 3 &&
             floorDecimal (-14.2) == (-15) &&
             floorDecimal (-7.8) == (-8)
               then putStrLn "Works!"
               else putStrLn "Does not work."

          putStr "7. isNotALetter: "
          if isNotALetter '1' == True &&
             isNotALetter '@' == True &&
             isNotALetter ',' == True &&
             isNotALetter '[' == True &&
             isNotALetter 'A' == False &&
             isNotALetter 'a' == False &&
             isNotALetter 'Z' == False &&
             isNotALetter 'z' == False &&
             isNotALetter 'C' == False &&
             isNotALetter 'x' == False
               then putStrLn "Works!"
               else putStrLn "Does not work."

          putStr "8. letterGrade: "
          if letterGrade 0 == "F" &&
             letterGrade 59 == "F" &&
             letterGrade 60 == "D-" &&
             letterGrade 63 == "D" &&
             letterGrade 67 == "D+" &&
             letterGrade 70 == "C-" &&
             letterGrade 73 == "C" &&
             letterGrade 77 == "C+" &&
             letterGrade 80 == "B-" &&
             letterGrade 83 == "B" &&
             letterGrade 87 == "B+" &&
             letterGrade 90 == "A-" &&
             letterGrade 93 == "A" &&
             letterGrade 100 == "A"
               then putStrLn "Works!"
               else putStrLn "Does not work."

          putStr "9. averageThree: "
          if averageThree 1 1 1 == 1.0 &&
             averageThree 42 42 42 == 42.0 &&
             averageThree 12 4 5 == 7.0 &&
             averageThree (-4) 10 (-3) == 1.0 &&
             averageThree (-4) (-10) (-7) == (-7.0)
               then putStrLn "Works!"
               else putStrLn "Does not work."

          putStr "10. howManyBelowAverage: "
          if howManyBelowAverage 1 1 1 == 0 &&
             howManyBelowAverage (-42) (-42) (-42) == 0 &&
             howManyBelowAverage 30 40 50 == 1 &&
             howManyBelowAverage (-4) (-10) (-7) == 1 &&
             howManyBelowAverage 100 40 30 == 2 &&
             howManyBelowAverage 2 2 3 == 2 &&
             howManyBelowAverage (-100) (-40) 3000 == 2
               then putStrLn "Works!"
               else putStrLn "Does not work."
