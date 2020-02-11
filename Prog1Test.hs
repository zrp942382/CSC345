{-
"Prog1Test.hs" - Test Cases for HW 1 -> Prog1.hs
West Chester University - CSC 345 - Programming Language Concepts / Paradigms - Spring 2020
Original format provided by: Richard Burns , distributed with permission.
Custom additions & modifications in 2019 for Prog1 by: Mahmoud Gudarzi, Anton Adamovich, and Akash Kumar
Custom additions & modifications in 2020 for Prog1 by: Zachary Perales
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
		  
          putStr "1. isPositive: "  -- Modified Test. 
          if isPositive 0 == True &&
             isPositive 1 == True &&
             isPositive (-1) == False
               then putStrLn "Works!"
               else putStrLn "Does not work."
			   
          putStr "2. dividesEvenlyByFive: "  -- Modified Test.
          if dividesEvenlyByFive 10 == True &&
             dividesEvenlyByFive (-10) == True &&
             dividesEvenlyByFive 0 == True &&  -- Should probably return true, but 0 isn't being tested. Remove this line if you want.
             dividesEvenlyByFive 11 == False &&
             dividesEvenlyByFive (-11)== False
               then putStrLn "Works!"
               else putStrLn "Does not work."
			  
          putStr "3. middle: " -- Test wasn't modified from the original test. 
          if middle 3 6 9 == 6 &&
             middle 6 3 9 == 6 &&
             middle 9 3 6 == 6 &&
             middle 3 9 6 == 6 &&
             middle 6 9 3 == 6 &&
             middle 9 6 3 == 6 &&
             middle (-3) (-6) (-9) == (-6) &&
             middle (-6) (-3) (-9) == (-6) &&
             middle (-9) (-3) (-6) == (-6) &&
             middle (-3) (-9) (-6) == (-6) &&
             middle (-6) (-9) (-3) == (-6) &&
             middle (-9) (-6) (-3) == (-6) &&
             middle 10 1 2 == 2 &&
             middle 1 10 2 == 2 &&
             middle 2 10 1 == 2 &&
             middle 10 2 1 == 2 &&
             middle 1 2 10 == 2 &&
             middle 2 1 10 == 2 &&
             middle (-10) (-1) (-2) == (-2) &&
             middle (-1) (-10) (-2) == (-2) &&
             middle (-2) (-10) (-1) == (-2) &&
             middle (-10) (-2) (-1) == (-2) &&
             middle (-1) (-2) (-10) == (-2) &&
             middle (-2) (-1) (-10) == (-2) &&
             middle 100 1000 10000 == 1000 &&
             middle 1000 100 10000 == 1000 &&
             middle 10000 100 1000 == 1000 &&
             middle 100 10000 1000 == 1000 &&
             middle 1000 10000 100 == 1000 &&
             middle 10000 1000 100 == 1000 &&
             middle (-100) (-1000) (-10000) == (-1000) &&
             middle (-1000) (-100) (-10000) == (-1000) &&
             middle (-10000) (-100) (-1000) == (-1000) &&
             middle (-100) (-10000) (-1000) == (-1000) &&
             middle (-1000) (-10000) (-100) == (-1000) &&
             middle (-10000) (-1000) (-100) == (-1000) &&
             middle 3 3 3 == 3 &&
             middle 2 3 3 == 3 &&
             middle 3 2 3 == 3 &&
             middle 3 3 2 == 3 &&
             middle 3 2 2 == 2 &&
             middle 2 3 2 == 2 &&
             middle 2 2 3 == 2 &&
             middle (-3) (-3) (-3) == (-3) &&
             middle (-2) (-3) (-3) == (-3) &&
             middle (-3) (-2) (-3) == (-3) &&
             middle (-3) (-3) (-2) == (-3) &&
             middle (-3) (-2) (-2) == (-2) &&
             middle (-2) (-3) (-2) == (-2) &&
             middle (-2) (-2) (-3) == (-2)
               then putStrLn "Works!"
               else putStrLn "Does not work."

          putStr "4. nor: " -- Modified Test.
          if nor False False == True &&
             nor False True == False &&
             nor True False == False &&
             nor True True == False
               then putStrLn "Works!"
               else putStrLn "Does not work."

          putStr "5. triangleArea: " -- Test wasn't modified from the original test.
          if triangleArea 2 4 == 4.0 &&
             triangleArea 2 2 == 2.0 &&
             triangleArea 3 5 == 7.5 && -- Remove "&&" here if and only if you remove the last three lines. 
             triangleArea (-7) 4 == (-14.0) && -- Not being tested for, this wouldn't be a triangle. Remove this line if you want.
             triangleArea 3 (-9) == (-13.5) && -- Not being tested for, this wouldn't be a triangle. Remove this line if you want.
             triangleArea (-10) (-2000) == 10000.0 -- Not being tested for, this wouldn't be a triangle. Remove this line if you want.
               then putStrLn "Works!"
               else putStrLn "Does not work."

          putStr "6. ceilingDecimal: " -- Modified Test.
          if ceilingDecimal 2.0 == 2.0 &&
             ceilingDecimal 2.1 == 3.0 &&
             ceilingDecimal 0.0 == 0.0 && 
             ceilingDecimal (-2.0) == (-2.0) &&
             ceilingDecimal (-2.1) == (-2.0)
               then putStrLn "Works!"
               else putStrLn "Does not work."

          putStr "7. letterGrade: "  -- Test wasn't modified from the original test.
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

          putStr "8. averageThree: "  -- Test wasn't modified from the original test.
          if averageThree 1 1 1 == 1.0 &&
             averageThree 42 42 42 == 42.0 &&
             averageThree 12 4 5 == 7.0 &&
             averageThree (-4) 10 (-3) == 1.0 &&
             averageThree (-4) (-10) (-7) == (-7.0)
               then putStrLn "Works!"
               else putStrLn "Does not work."

          putStr "9. howManyAboveAverage: " -- Modified Test.
          if howManyAboveAverage 3 6 9 == 1 &&
             howManyAboveAverage 6 3 9 == 1 &&
             howManyAboveAverage 9 3 6 == 1 &&
             howManyAboveAverage 3 9 6 == 1 &&
             howManyAboveAverage 6 9 3 == 1 &&
             howManyAboveAverage 9 6 3 == 1 &&
             howManyAboveAverage (-3) (-6) (-9) == 1 &&
             howManyAboveAverage (-6) (-3) (-9) == 1 &&
             howManyAboveAverage (-9) (-3) (-6) == 1 &&
             howManyAboveAverage (-3) (-9) (-6) == 1 &&
             howManyAboveAverage (-6) (-9) (-3) == 1 &&
             howManyAboveAverage (-9) (-6) (-3) == 1 &&
             howManyAboveAverage 10 1 2 == 1 &&
             howManyAboveAverage 1 10 2 == 1 &&
             howManyAboveAverage 2 10 1 == 1 &&
             howManyAboveAverage 10 2 1 == 1 &&
             howManyAboveAverage 1 2 10 == 1 &&
             howManyAboveAverage 2 1 10 == 1 &&
             howManyAboveAverage (-10) (-1) (-2) == 2 &&
             howManyAboveAverage (-1) (-10) (-2) == 2 &&
             howManyAboveAverage (-2) (-10) (-1) == 2 &&
             howManyAboveAverage (-10) (-2) (-1) == 2 &&
             howManyAboveAverage (-1) (-2) (-10) == 2 &&
             howManyAboveAverage (-2) (-1) (-10) == 2 &&
             howManyAboveAverage 100 1000 10000 == 1 &&
             howManyAboveAverage 1000 100 10000 == 1 &&
             howManyAboveAverage 10000 100 1000 == 1 &&
             howManyAboveAverage 100 10000 1000 == 1 &&
             howManyAboveAverage 1000 10000 100 == 1 &&
             howManyAboveAverage 10000 1000 100 == 1 &&
             howManyAboveAverage (-100) (-1000) (-10000) == 2 &&
             howManyAboveAverage (-1000) (-100) (-10000) == 2 &&
             howManyAboveAverage (-10000) (-100) (-1000) == 2 &&
             howManyAboveAverage (-100) (-10000) (-1000) == 2 &&
             howManyAboveAverage (-1000) (-10000) (-100) == 2 &&
             howManyAboveAverage (-10000) (-1000) (-100) == 2 &&
             howManyAboveAverage 3 3 3 == 0 &&
             howManyAboveAverage 2 3 3 == 2 &&
             howManyAboveAverage 3 2 3 == 2 &&
             howManyAboveAverage 3 3 2 == 2 &&
             howManyAboveAverage 3 2 2 == 1 &&
             howManyAboveAverage 2 3 2 == 1 &&
             howManyAboveAverage 2 2 3 == 1 &&
             howManyAboveAverage (-3) (-3) (-3) == 0 &&
             howManyAboveAverage (-2) (-3) (-3) == 1 &&
             howManyAboveAverage (-3) (-2) (-3) == 1 &&
             howManyAboveAverage (-3) (-3) (-2) == 1 &&
             howManyAboveAverage (-3) (-2) (-2) == 2 &&
             howManyAboveAverage (-2) (-3) (-2) == 2 &&
             howManyAboveAverage (-2) (-2) (-3) == 2 &&
             howManyAboveAverage 0 0 0 == 0
               then putStrLn "Works!"
               else putStrLn "Does not work."
			   
          putStr "10. howManyWithinThreshold: " -- New test.
          if howManyWithinThreshold 3 6 9 3.0 == 1 &&
             howManyWithinThreshold 6 3 9 3.0 == 1 &&
             howManyWithinThreshold 9 3 6 3.0 == 1 &&
             howManyWithinThreshold 3 9 6 3.0 == 1 &&
             howManyWithinThreshold 6 9 3 3.0 == 1 &&
             howManyWithinThreshold 9 6 3 3.0 == 1 &&
             howManyWithinThreshold (-3) (-6) (-9) 3.0 == 1 &&
             howManyWithinThreshold (-6) (-3) (-9) 3.0 == 1 &&
             howManyWithinThreshold (-9) (-3) (-6) 3.0 == 1 &&
             howManyWithinThreshold (-3) (-9) (-6) 3.0 == 1 &&
             howManyWithinThreshold (-6) (-9) (-3) 3.0 == 1 &&
             howManyWithinThreshold (-9) (-6) (-3) 3.0 == 1 &&
             howManyWithinThreshold 3 6 9 3.1 == 3 &&
             howManyWithinThreshold 6 3 9 3.1 == 3 &&
             howManyWithinThreshold 9 3 6 3.1 == 3 &&
             howManyWithinThreshold 3 9 6 3.1 == 3 &&
             howManyWithinThreshold 6 9 3 3.1 == 3 &&
             howManyWithinThreshold 9 6 3 3.1 == 3 &&
             howManyWithinThreshold (-3) (-6) (-9) 3.1 == 3 &&
             howManyWithinThreshold (-6) (-3) (-9) 3.1 == 3 &&
             howManyWithinThreshold (-9) (-3) (-6) 3.1 == 3 &&
             howManyWithinThreshold (-3) (-9) (-6) 3.1 == 3 &&
             howManyWithinThreshold (-6) (-9) (-3) 3.1 == 3 &&
             howManyWithinThreshold (-9) (-6) (-3) 3.1 == 3 &&
             howManyWithinThreshold 10 1 2 3.5 == 2 &&
             howManyWithinThreshold 1 10 2 3.5 == 2 &&
             howManyWithinThreshold 2 10 1 3.5 == 2 &&
             howManyWithinThreshold 10 2 1 3.5 == 2 &&
             howManyWithinThreshold 1 2 10 3.5 == 2 &&
             howManyWithinThreshold 2 1 10 3.5 == 2 &&
             howManyWithinThreshold (-10) (-1) (-2) 3.5 == 2 &&
             howManyWithinThreshold (-1) (-10) (-2) 3.5 == 2 &&
             howManyWithinThreshold (-2) (-10) (-1) 3.5 == 2 &&
             howManyWithinThreshold (-10) (-2) (-1) 3.5 == 2 &&
             howManyWithinThreshold (-1) (-2) (-10) 3.5 == 2 &&
             howManyWithinThreshold (-2) (-1) (-10) 3.5 == 2 &&
             howManyWithinThreshold 100 1000 10000 10.0 == 0 &&
             howManyWithinThreshold 1000 100 10000 10.0 == 0 &&
             howManyWithinThreshold 10000 100 1000 10.0 == 0 &&
             howManyWithinThreshold 100 10000 1000 10.0 == 0 &&
             howManyWithinThreshold 1000 10000 100 10.0 == 0 &&
             howManyWithinThreshold 10000 1000 100 10.0 == 0 &&
             howManyWithinThreshold (-100) (-1000) (-10000) 10.0 == 0 &&
             howManyWithinThreshold (-1000) (-100) (-10000) 10.0 == 0 &&
             howManyWithinThreshold (-10000) (-100) (-1000) 10.0 == 0 &&
             howManyWithinThreshold (-100) (-10000) (-1000) 10.0 == 0 &&
             howManyWithinThreshold (-1000) (-10000) (-100) 10.0 == 0 &&
             howManyWithinThreshold (-10000) (-1000) (-100) 10.0 == 0 
               then putStrLn "Works!"
               else putStrLn "Does not work."