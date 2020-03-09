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
module Prog4Test where
import Prog4
import Data.List (insert)

insertionSort :: Ord a => [a] -> [a]
insertionSort = foldr insert []

takeFst :: [(Int, String)] -> [Int]
takeFst x = if (x == []) then [] else fst (head x) : takeFst (tail x)

main :: IO ()
main = do
          putStrLn "Testing Prog4: "
          
          putStr "1. doubleAll: " 
          if   doubleAll	[ 	-5	,	-5	,	-5	]	==	[(-5,-10)	,	(-5,-10)	,	(-5,-10)]	&&
               doubleAll	[	-5	,	-5	,	-1	]	==	[(-5,-10)	,	(-5,-10)	,	(-1,-2)]	&&
               doubleAll	[	-5	,	-5	,	0	]	==	[(-5,-10)	,	(-5,-10)	,	(0,0)]	&&
               doubleAll	[	-5	,	-5	,	1	]	==	[(-5,-10)	,	(-5,-10)	,	(1,2)]	&&
               doubleAll	[	-5	,	-5	,	5	]	==	[(-5,-10)	,	(-5,-10)	,	(5,10)]	&&
               doubleAll	[	-5	,	-1	,	-5	]	==	[(-5,-10)	,	(-1,-2)	,	(-5,-10)]	&&
               doubleAll	[	-5	,	-1	,	-1	]	==	[(-5,-10)	,	(-1,-2)	,	(-1,-2)]	&&
               doubleAll	[	-5	,	-1	,	0	]	==	[(-5,-10)	,	(-1,-2)	,	(0,0)]	&&
               doubleAll	[	-5	,	-1	,	1	]	==	[(-5,-10)	,	(-1,-2)	,	(1,2)]	&&
               doubleAll	[	-5	,	-1	,	5	]	==	[(-5,-10)	,	(-1,-2)	,	(5,10)]	&&
               doubleAll	[	-5	,	0	,	-5	]	==	[(-5,-10)	,	(0,0)	,	(-5,-10)]	&&
               doubleAll	[	-5	,	0	,	-1	]	==	[(-5,-10)	,	(0,0)	,	(-1,-2)]	&&
               doubleAll	[	-5	,	0	,	0	]	==	[(-5,-10)	,	(0,0)	,	(0,0)]	&&
               doubleAll	[	-5	,	0	,	1	]	==	[(-5,-10)	,	(0,0)	,	(1,2)]	&&
               doubleAll	[	-5	,	0	,	5	]	==	[(-5,-10)	,	(0,0)	,	(5,10)]	&&
               doubleAll	[	-5	,	1	,	-5	]	==	[(-5,-10)	,	(1,2)	,	(-5,-10)]	&&
               doubleAll	[	-5	,	1	,	-1	]	==	[(-5,-10)	,	(1,2)	,	(-1,-2)]	&&
               doubleAll	[	-5	,	1	,	0	]	==	[(-5,-10)	,	(1,2)	,	(0,0)]	&&
               doubleAll	[	-5	,	1	,	1	]	==	[(-5,-10)	,	(1,2)	,	(1,2)]	&&
               doubleAll	[	-5	,	1	,	5	]	==	[(-5,-10)	,	(1,2)	,	(5,10)]	&&
               doubleAll	[	-5	,	5	,	-5	]	==	[(-5,-10)	,	(5,10)	,	(-5,-10)]	&&
               doubleAll	[	-5	,	5	,	-1	]	==	[(-5,-10)	,	(5,10)	,	(-1,-2)]	&&
               doubleAll	[	-5	,	5	,	0	]	==	[(-5,-10)	,	(5,10)	,	(0,0)]	&&
               doubleAll	[	-5	,	5	,	1	]	==	[(-5,-10)	,	(5,10)	,	(1,2)]	&&
               doubleAll	[	-5	,	5	,	5	]	==	[(-5,-10)	,	(5,10)	,	(5,10)]	&&
               doubleAll	[	-1	,	-5	,	-5	]	==	[(-1,-2)	,	(-5,-10)	,	(-5,-10)]	&&
               doubleAll	[	-1	,	-5	,	-1	]	==	[(-1,-2)	,	(-5,-10)	,	(-1,-2)]	&&
               doubleAll	[	-1	,	-5	,	0	]	==	[(-1,-2)	,	(-5,-10)	,	(0,0)]	&&
               doubleAll	[	-1	,	-5	,	1	]	==	[(-1,-2)	,	(-5,-10)	,	(1,2)]	&&
               doubleAll	[	-1	,	-5	,	5	]	==	[(-1,-2)	,	(-5,-10)	,	(5,10)]	&&
               doubleAll	[	-1	,	-1	,	-5	]	==	[(-1,-2)	,	(-1,-2)	,	(-5,-10)]	&&
               doubleAll	[	-1	,	-1	,	-1	]	==	[(-1,-2)	,	(-1,-2)	,	(-1,-2)]	&&
               doubleAll	[	-1	,	-1	,	0	]	==	[(-1,-2)	,	(-1,-2)	,	(0,0)]	&&
               doubleAll	[	-1	,	-1	,	1	]	==	[(-1,-2)	,	(-1,-2)	,	(1,2)]	&&
               doubleAll	[	-1	,	-1	,	5	]	==	[(-1,-2)	,	(-1,-2)	,	(5,10)]	&&
               doubleAll	[	-1	,	0	,	-5	]	==	[(-1,-2)	,	(0,0)	,	(-5,-10)]	&&
               doubleAll	[	-1	,	0	,	-1	]	==	[(-1,-2)	,	(0,0)	,	(-1,-2)]	&&
               doubleAll	[	-1	,	0	,	0	]	==	[(-1,-2)	,	(0,0)	,	(0,0)]	&&
               doubleAll	[	-1	,	0	,	1	]	==	[(-1,-2)	,	(0,0)	,	(1,2)]	&&
               doubleAll	[	-1	,	0	,	5	]	==	[(-1,-2)	,	(0,0)	,	(5,10)]	&&
               doubleAll	[	-1	,	1	,	-5	]	==	[(-1,-2)	,	(1,2)	,	(-5,-10)]	&&
               doubleAll	[	-1	,	1	,	-1	]	==	[(-1,-2)	,	(1,2)	,	(-1,-2)]	&&
               doubleAll	[	-1	,	1	,	0	]	==	[(-1,-2)	,	(1,2)	,	(0,0)]	&&
               doubleAll	[	-1	,	1	,	1	]	==	[(-1,-2)	,	(1,2)	,	(1,2)]	&&
               doubleAll	[	-1	,	1	,	5	]	==	[(-1,-2)	,	(1,2)	,	(5,10)]	&&
               doubleAll	[	-1	,	5	,	-5	]	==	[(-1,-2)	,	(5,10)	,	(-5,-10)]	&&
               doubleAll	[	-1	,	5	,	-1	]	==	[(-1,-2)	,	(5,10)	,	(-1,-2)]	&&
               doubleAll	[	-1	,	5	,	0	]	==	[(-1,-2)	,	(5,10)	,	(0,0)]	&&
               doubleAll	[	-1	,	5	,	1	]	==	[(-1,-2)	,	(5,10)	,	(1,2)]	&&
               doubleAll	[	-1	,	5	,	5	]	==	[(-1,-2)	,	(5,10)	,	(5,10)]	&&
               doubleAll	[	0	,	-5	,	-5	]	==	[(0,0)	,	(-5,-10)	,	(-5,-10)]	&&
               doubleAll	[	0	,	-5	,	-1	]	==	[(0,0)	,	(-5,-10)	,	(-1,-2)]	&&
               doubleAll	[	0	,	-5	,	0	]	==	[(0,0)	,	(-5,-10)	,	(0,0)]	&&
               doubleAll	[	0	,	-5	,	1	]	==	[(0,0)	,	(-5,-10)	,	(1,2)]	&&
               doubleAll	[	0	,	-5	,	5	]	==	[(0,0)	,	(-5,-10)	,	(5,10)]	&&
               doubleAll	[	0	,	-1	,	-5	]	==	[(0,0)	,	(-1,-2)	,	(-5,-10)]	&&
               doubleAll	[	0	,	-1	,	-1	]	==	[(0,0)	,	(-1,-2)	,	(-1,-2)]	&&
               doubleAll	[	0	,	-1	,	0	]	==	[(0,0)	,	(-1,-2)	,	(0,0)]	&&
               doubleAll	[	0	,	-1	,	1	]	==	[(0,0)	,	(-1,-2)	,	(1,2)]	&&
               doubleAll	[	0	,	-1	,	5	]	==	[(0,0)	,	(-1,-2)	,	(5,10)]	&&
               doubleAll	[	0	,	0	,	-5	]	==	[(0,0)	,	(0,0)	,	(-5,-10)]	&&
               doubleAll	[	0	,	0	,	-1	]	==	[(0,0)	,	(0,0)	,	(-1,-2)]	&&
               doubleAll	[	0	,	0	,	0	]	==	[(0,0)	,	(0,0)	,	(0,0)]	&&
               doubleAll	[	0	,	0	,	1	]	==	[(0,0)	,	(0,0)	,	(1,2)]	&&
               doubleAll	[	0	,	0	,	5	]	==	[(0,0)	,	(0,0)	,	(5,10)]	&&
               doubleAll	[	0	,	1	,	-5	]	==	[(0,0)	,	(1,2)	,	(-5,-10)]	&&
               doubleAll	[	0	,	1	,	-1	]	==	[(0,0)	,	(1,2)	,	(-1,-2)]	&&
               doubleAll	[	0	,	1	,	0	]	==	[(0,0)	,	(1,2)	,	(0,0)]	&&
               doubleAll	[	0	,	1	,	1	]	==	[(0,0)	,	(1,2)	,	(1,2)]	&&
               doubleAll	[	0	,	1	,	5	]	==	[(0,0)	,	(1,2)	,	(5,10)]	&&
               doubleAll	[	0	,	5	,	-5	]	==	[(0,0)	,	(5,10)	,	(-5,-10)]	&&
               doubleAll	[	0	,	5	,	-1	]	==	[(0,0)	,	(5,10)	,	(-1,-2)]	&&
               doubleAll	[	0	,	5	,	0	]	==	[(0,0)	,	(5,10)	,	(0,0)]	&&
               doubleAll	[	0	,	5	,	1	]	==	[(0,0)	,	(5,10)	,	(1,2)]	&&
               doubleAll	[	0	,	5	,	5	]	==	[(0,0)	,	(5,10)	,	(5,10)]	&&
               doubleAll	[	1	,	-5	,	-5	]	==	[(1,2)	,	(-5,-10)	,	(-5,-10)]	&&
               doubleAll	[	1	,	-5	,	-1	]	==	[(1,2)	,	(-5,-10)	,	(-1,-2)]	&&
               doubleAll	[	1	,	-5	,	0	]	==	[(1,2)	,	(-5,-10)	,	(0,0)]	&&
               doubleAll	[	1	,	-5	,	1	]	==	[(1,2)	,	(-5,-10)	,	(1,2)]	&&
               doubleAll	[	1	,	-5	,	5	]	==	[(1,2)	,	(-5,-10)	,	(5,10)]	&&
               doubleAll	[	1	,	-1	,	-5	]	==	[(1,2)	,	(-1,-2)	,	(-5,-10)]	&&
               doubleAll	[	1	,	-1	,	-1	]	==	[(1,2)	,	(-1,-2)	,	(-1,-2)]	&&
               doubleAll	[	1	,	-1	,	0	]	==	[(1,2)	,	(-1,-2)	,	(0,0)]	&&
               doubleAll	[	1	,	-1	,	1	]	==	[(1,2)	,	(-1,-2)	,	(1,2)]	&&
               doubleAll	[	1	,	-1	,	5	]	==	[(1,2)	,	(-1,-2)	,	(5,10)]	&&
               doubleAll	[	1	,	0	,	-5	]	==	[(1,2)	,	(0,0)	,	(-5,-10)]	&&
               doubleAll	[	1	,	0	,	-1	]	==	[(1,2)	,	(0,0)	,	(-1,-2)]	&&
               doubleAll	[	1	,	0	,	0	]	==	[(1,2)	,	(0,0)	,	(0,0)]	&&
               doubleAll	[	1	,	0	,	1	]	==	[(1,2)	,	(0,0)	,	(1,2)]	&&
               doubleAll	[	1	,	0	,	5	]	==	[(1,2)	,	(0,0)	,	(5,10)]	&&
               doubleAll	[	1	,	1	,	-5	]	==	[(1,2)	,	(1,2)	,	(-5,-10)]	&&
               doubleAll	[	1	,	1	,	-1	]	==	[(1,2)	,	(1,2)	,	(-1,-2)]	&&
               doubleAll	[	1	,	1	,	0	]	==	[(1,2)	,	(1,2)	,	(0,0)]	&&
               doubleAll	[	1	,	1	,	1	]	==	[(1,2)	,	(1,2)	,	(1,2)]	&&
               doubleAll	[	1	,	1	,	5	]	==	[(1,2)	,	(1,2)	,	(5,10)]	&&
               doubleAll	[	1	,	5	,	-5	]	==	[(1,2)	,	(5,10)	,	(-5,-10)]	&&
               doubleAll	[	1	,	5	,	-1	]	==	[(1,2)	,	(5,10)	,	(-1,-2)]	&&
               doubleAll	[	1	,	5	,	0	]	==	[(1,2)	,	(5,10)	,	(0,0)]	&&
               doubleAll	[	1	,	5	,	1	]	==	[(1,2)	,	(5,10)	,	(1,2)]	&&
               doubleAll	[	1	,	5	,	5	]	==	[(1,2)	,	(5,10)	,	(5,10)]	&&
               doubleAll	[	5	,	-5	,	-5	]	==	[(5,10)	,	(-5,-10)	,	(-5,-10)]	&&
               doubleAll	[	5	,	-5	,	-1	]	==	[(5,10)	,	(-5,-10)	,	(-1,-2)]	&&
               doubleAll	[	5	,	-5	,	0	]	==	[(5,10)	,	(-5,-10)	,	(0,0)]	&&
               doubleAll	[	5	,	-5	,	1	]	==	[(5,10)	,	(-5,-10)	,	(1,2)]	&&
               doubleAll	[	5	,	-5	,	5	]	==	[(5,10)	,	(-5,-10)	,	(5,10)]	&&
               doubleAll	[	5	,	-1	,	-5	]	==	[(5,10)	,	(-1,-2)	,	(-5,-10)]	&&
               doubleAll	[	5	,	-1	,	-1	]	==	[(5,10)	,	(-1,-2)	,	(-1,-2)]	&&
               doubleAll	[	5	,	-1	,	0	]	==	[(5,10)	,	(-1,-2)	,	(0,0)]	&&
               doubleAll	[	5	,	-1	,	1	]	==	[(5,10)	,	(-1,-2)	,	(1,2)]	&&
               doubleAll	[	5	,	-1	,	5	]	==	[(5,10)	,	(-1,-2)	,	(5,10)]	&&
               doubleAll	[	5	,	0	,	-5	]	==	[(5,10)	,	(0,0)	,	(-5,-10)]	&&
               doubleAll	[	5	,	0	,	-1	]	==	[(5,10)	,	(0,0)	,	(-1,-2)]	&&
               doubleAll	[	5	,	0	,	0	]	==	[(5,10)	,	(0,0)	,	(0,0)]	&&
               doubleAll	[	5	,	0	,	1	]	==	[(5,10)	,	(0,0)	,	(1,2)]	&&
               doubleAll	[	5	,	0	,	5	]	==	[(5,10)	,	(0,0)	,	(5,10)]	&&
               doubleAll	[	5	,	1	,	-5	]	==	[(5,10)	,	(1,2)	,	(-5,-10)]	&&
               doubleAll	[	5	,	1	,	-1	]	==	[(5,10)	,	(1,2)	,	(-1,-2)]	&&
               doubleAll	[	5	,	1	,	0	]	==	[(5,10)	,	(1,2)	,	(0,0)]	&&
               doubleAll	[	5	,	1	,	1	]	==	[(5,10)	,	(1,2)	,	(1,2)]	&&
               doubleAll	[	5	,	1	,	5	]	==	[(5,10)	,	(1,2)	,	(5,10)]	&&
               doubleAll	[	5	,	5	,	-5	]	==	[(5,10)	,	(5,10)	,	(-5,-10)]	&&
               doubleAll	[	5	,	5	,	-1	]	==	[(5,10)	,	(5,10)	,	(-1,-2)]	&&
               doubleAll	[	5	,	5	,	0	]	==	[(5,10)	,	(5,10)	,	(0,0)]	&&
               doubleAll	[	5	,	5	,	1	]	==	[(5,10)	,	(5,10)	,	(1,2)]	&&
               doubleAll	[	5	,	5	,	5	]	==	[(5,10)	,	(5,10)	,	(5,10)]	&&
               doubleAll	[	-5	,	-5	,	-5	]	==	[(-5,-10)	,	(-5,-10)	,	(-5,-10)]
          then putStrLn "Works!"
          else putStrLn "Does not work."

          putStr "2. productLastPart: "
          if   productLastPart 0 [-2,-1,0,1,2] == 1 &&
               productLastPart 1 [2] == 2 &&
               productLastPart 4 [0,-2,-1,1,2] == 4 &&
               productLastPart 5 [0,-2,-1,1,2] == 0
          then putStrLn "Works!"
          else putStrLn "Does not work."

          putStr "3. init': "
          if   init' [1] == init [1] &&
               init' [1,2] == init [1,2] &&
               init' [1,2,3] == init [1,2,3] &&
               init' [1,2,3,4] == init [1,2,3,4] &&
               init' [1,2,3,4,5] == init [1,2,3,4,5]
          then putStrLn "Works!"
          else putStrLn "Does not work."

          putStr "4. lowerOddLetters: "
          if   lowerOddLetters "CrocodilE" == "crocodile" &&
               lowerOddLetters "crocodile" == "crocodile" &&
               lowerOddLetters "CROCODILE" == "cRoCoDiLe" &&
               lowerOddLetters "CROCODILEE" == "cRoCoDiLeE" &&
               lowerOddLetters "ABC" == "aBc" &&
               lowerOddLetters "AB" == "aB" &&
               lowerOddLetters "A" == "a" &&
               lowerOddLetters "a" == "a" &&
               lowerOddLetters "" == "" 
          then putStrLn "Works!"
          else putStrLn "Does not work."

          putStr "5. replicate': "
          if   replicate' 0 'A' == replicate 0 'A' &&
               replicate' 1 '&' == replicate 1 '&' &&
               replicate' 2 '^' == replicate 2 '^' &&
               replicate' 3 'a' == replicate 3 'a' &&
               replicate' 4 '+' == replicate 4 '+' &&
               replicate' 5 ' ' == replicate 5 ' ' 
          then putStrLn "Works!"
          else putStrLn "Does not work."

          putStr "6. iSort: "
          if   takeFst (iSort' [(5, "abc"), (4, "abc"), (3, "abc"), (2, "cba"), (1, "cdb")]) == insertionSort [5, 4, 3, 2, 1] &&
               takeFst (iSort' [(-1, "abc"), (-2, "abc"), (-3, "abc"), (-4, "cba"), (-5, "cdb")]) == insertionSort [-1, -2, -3, -4, -5] &&
               takeFst (iSort' [(-1, "abc"), (-2, "abc"), (-3, "abc"), (-4, "cba"), (-5, "cdb")]) == insertionSort [-1, -2, -3, -4, -5] &&
               takeFst (iSort' [(0, "abc")]) == insertionSort [0] &&
               takeFst (iSort' []) == insertionSort []
          then putStrLn "Works!"
          else putStrLn "Does not work."

          putStr "7. lowerFirstCharacter: "
          if   lowerFirstCharacter "Crocodile" == "crocodile" &&
               lowerFirstCharacter " " == " " &&
               lowerFirstCharacter "" == "" &&
               lowerFirstCharacter "D" == "d" &&
               lowerFirstCharacter " D" == " D" &&
               lowerFirstCharacter " *^~ C *^~ c *^~ D *^~ E *^~" == " *^~ C *^~ c *^~ D *^~ E *^~" &&
               lowerFirstCharacter "E *^~ C *^~ c *^~ D *^~ E *^~" == "e *^~ C *^~ c *^~ D *^~ E *^~" &&
               lowerFirstCharacter "+^Cro+^codile" == "+^Cro+^codile" && 
               lowerFirstCharacter "crocodile" == "crocodile"
          then putStrLn "Works!"
          else putStrLn "Does not work."

          putStr "8. middleWord: "
          if   middleWord "Haskell is fun!" == "is" &&
               middleWord "r q p" == "q" &&
               middleWord "pp# is q" == "is" &&
               middleWord "_ __ $__" == "__" &&
               middleWord "#&@* 223* ((3!" == "223*"
          then putStrLn "Works!"
          else putStrLn "Does not work."

          putStr "9. lowerFirstLetter: "
          if   lowerFirstLetter "crocodile" == "crocodile" &&
               lowerFirstLetter " " == " " &&
               lowerFirstLetter "" == "" &&
               lowerFirstLetter "##P" == "##p" &&
               lowerFirstLetter "  #PP" == "  #pP" &&
               lowerFirstLetter " *^~ C *^~ c *^~ D *^~ E *^~" == " *^~ c *^~ c *^~ D *^~ E *^~" &&
               lowerFirstLetter "+^crOc+^oDile+^" == "+^croc+^oDile+^" && 
               lowerFirstLetter "cRoCodilE" == "croCodilE"
          then putStrLn "Works!"
          else putStrLn "Does not work."

          putStr "10. lowerFirstTwoLetters: "
          if   lowerFirstTwoLetters "CRocOdilE" == "crocOdilE" &&
               lowerFirstTwoLetters " " == " " &&
               lowerFirstTwoLetters " *^~ C *^~ c *^~ D *^~ E *^~" == " *^~ c *^~ c *^~ d *^~ E *^~" &&
               lowerFirstTwoLetters "+^crOc+^oDilE+^" == "+^croc+^odilE+^" && 
               lowerFirstTwoLetters "cRoCodilE" == "crocodilE" &&
               lowerFirstTwoLetters "cRo+^coDilE" == "cro+^codilE"
          then putStrLn "Works!"
          else putStrLn "Does not work."