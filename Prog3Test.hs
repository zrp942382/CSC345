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
module Prog3Test where
import Prog3

checkTriads :: [(Int, Int, Int)] -> [(Int, Int, Int)] -> Bool
checkTriads (x:xs) y = if (length xs == 0) then True else if (elem x y) then checkTriads xs y else False

main :: IO ()
main = do
          putStrLn "Testing Prog2: "
          
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
          then putStrLn "Works! Check to make sure that you used list library functions or list comprehension and not recursion."
          else putStrLn "Does not work."

          putStr "2. productLastPart: "
          if   -- productLastPart 0 [-2,-1,0,1,2] == 0 && -- I don't really know why he would test for 0, but it's here anyway if you want to include the test.
               productLastPart 1 [2] == 2 &&
               productLastPart 4 [0,-2,-1,1,2] == 4 &&
               productLastPart 5 [0,-2,-1,1,2] == 0
          then putStrLn "Works! Check to make sure you used list library functions and not list comprehension or recursion."
          else putStrLn "Does not work."

          putStr "3. init': "
          if   init' [1] == init [1] &&
               init' [1,2] == init [1,2] &&
               init' [1,2,3] == init [1,2,3] &&
               init' [1,2,3,4] == init [1,2,3,4] &&
               init' [1,2,3,4,5] == init [1,2,3,4,5]
          then putStrLn "Works! Check to make sure you used list library functions and not list comprehension or recursion or init."
          else putStrLn "Does not work."

          putStr "4. nestedParens: "
          if   nestedParens "" == True && -- Still waiting to hear back from Dr. Burns on this.
               nestedParens "()" == True && -- Still waiting to hear back from Dr. Burns on this.
               nestedParens "(((((())))))" == True &&
               nestedParens "()()" == False &&
               nestedParens "(()))" == False
          then putStrLn "Works! Check to make sure you that YOU DID use recursion."
          else putStrLn "Does not work. You need to account for empty string and () for this test to work. Check testing comments."

          putStr "5. triads: "
          if   triads 100 == [(0,0,0),(0,1,1),(0,2,2),(0,3,3),(0,4,4),(0,5,5),(0,6,6),(0,7,7),(0,8,8),(0,9,9),(0,10,10),(0,11,11),(0,12,12),(0,13,13),(0,14,14),(0,15,15),(0,16,16),(0,17,17),
               (0,18,18),(0,19,19),(0,20,20),(0,21,21),(0,22,22),(0,23,23),(0,24,24),(0,25,25),(0,26,26),(0,27,27),(0,28,28),(0,29,29),(0,30,30),(0,31,31),(0,32,32),(0,33,33),(0,34,34),(0,35,35),
               (0,36,36),(0,37,37),(0,38,38),(0,39,39),(0,40,40),(0,41,41),(0,42,42),(0,43,43),(0,44,44),(0,45,45),(0,46,46),(0,47,47),(0,48,48),(0,49,49),(0,50,50),(0,51,51),(0,52,52),(0,53,53),
               (0,54,54),(0,55,55),(0,56,56),(0,57,57),(0,58,58),(0,59,59),(0,60,60),(0,61,61),(0,62,62),(0,63,63),(0,64,64),(0,65,65),(0,66,66),(0,67,67),(0,68,68),(0,69,69),(0,70,70),(0,71,71),
               (0,72,72),(0,73,73),(0,74,74),(0,75,75),(0,76,76),(0,77,77),(0,78,78),(0,79,79),(0,80,80),(0,81,81),(0,82,82),(0,83,83),(0,84,84),(0,85,85),(0,86,86),(0,87,87),(0,88,88),(0,89,89),
               (0,90,90),(0,91,91),(0,92,92),(0,93,93),(0,94,94),(0,95,95),(0,96,96),(0,97,97),(0,98,98),(0,99,99),(0,100,100),(1,0,1),(2,0,2),(3,0,3),(3,4,5),(4,0,4),(4,3,5),(5,0,5),(5,12,13),
               (6,0,6),(6,8,10),(7,0,7),(7,24,25),(8,0,8),(8,6,10),(8,15,17),(9,0,9),(9,12,15),(9,40,41),(10,0,10),(10,24,26),(11,0,11),(11,60,61),(12,0,12),(12,5,13),(12,9,15),(12,16,20),(12,35,37),
               (13,0,13),(13,84,85),(14,0,14),(14,48,50),(15,0,15),(15,8,17),(15,20,25),(15,36,39),(16,0,16),(16,12,20),(16,30,34),(16,63,65),(17,0,17),(18,0,18),(18,24,30),(18,80,82),(19,0,19),
               (20,0,20),(20,15,25),(20,21,29),(20,48,52),(21,0,21),(21,20,29),(21,28,35),(21,72,75),(22,0,22),(23,0,23),(24,0,24),(24,7,25),(24,10,26),(24,18,30),(24,32,40),(24,45,51),(24,70,74),
               (25,0,25),(25,60,65),(26,0,26),(27,0,27),(27,36,45),(28,0,28),(28,21,35),(28,45,53),(28,96,100),(29,0,29),(30,0,30),(30,16,34),(30,40,50),(30,72,78),(31,0,31),(32,0,32),(32,24,40),
               (32,60,68),(33,0,33),(33,44,55),(33,56,65),(34,0,34),(35,0,35),(35,12,37),(35,84,91),(36,0,36),(36,15,39),(36,27,45),(36,48,60),(36,77,85),(37,0,37),(38,0,38),(39,0,39),(39,52,65),
               (39,80,89),(40,0,40),(40,9,41),(40,30,50),(40,42,58),(40,75,85),(41,0,41),(42,0,42),(42,40,58),(42,56,70),(43,0,43),(44,0,44),(44,33,55),(45,0,45),(45,24,51),(45,28,53),(45,60,75),
               (46,0,46),(47,0,47),(48,0,48),(48,14,50),(48,20,52),(48,36,60),(48,55,73),(48,64,80),(49,0,49),(50,0,50),(51,0,51),(51,68,85),(52,0,52),(52,39,65),(53,0,53),(54,0,54),(54,72,90),
               (55,0,55),(55,48,73),(56,0,56),(56,33,65),(56,42,70),(57,0,57),(57,76,95),(58,0,58),(59,0,59),(60,0,60),(60,11,61),(60,25,65),(60,32,68),(60,45,75),(60,63,87),(60,80,100),(61,0,61),
               (62,0,62),(63,0,63),(63,16,65),(63,60,87),(64,0,64),(64,48,80),(65,0,65),(65,72,97),(66,0,66),(67,0,67),(68,0,68),(68,51,85),(69,0,69),(70,0,70),(70,24,74),(71,0,71),(72,0,72),
               (72,21,75),(72,30,78),(72,54,90),(72,65,97),(73,0,73),(74,0,74),(75,0,75),(75,40,85),(76,0,76),(76,57,95),(77,0,77),(77,36,85),(78,0,78),(79,0,79),(80,0,80),(80,18,82),(80,39,89),
               (80,60,100),(81,0,81),(82,0,82),(83,0,83),(84,0,84),(84,13,85),(84,35,91),(85,0,85),(86,0,86),(87,0,87),(88,0,88),(89,0,89),(90,0,90),(91,0,91),(92,0,92),(93,0,93),(94,0,94),(95,0,95),
               (96,0,96),(96,28,100),(97,0,97),(98,0,98), (99,0,99), (100,0,100)]
          then putStrLn "Works! Check to make sure you did not use recursion."
          else putStrLn "Does not work.\n This test is using the class to test my own code here.\nIf this test is failing, please notify me, your code may or may not be wrong.\nYou also must include a (0,0,0) base case for this test to work."

          putStr "EXTRA: checkTriads (list order does not matter): "
          if checkTriads (triads 100) [(0,0,0),(0,1,1),(0,2,2),(0,3,3),(0,4,4),(0,5,5),(0,6,6),(0,7,7),(0,8,8),(0,9,9),(0,10,10),(0,11,11),(0,12,12),(0,13,13),(0,14,14),(0,15,15),(0,16,16),(0,17,17),
               (0,18,18),(0,19,19),(0,20,20),(0,21,21),(0,22,22),(0,23,23),(0,24,24),(0,25,25),(0,26,26),(0,27,27),(0,28,28),(0,29,29),(0,30,30),(0,31,31),(0,32,32),(0,33,33),(0,34,34),(0,35,35),
               (0,36,36),(0,37,37),(0,38,38),(0,39,39),(0,40,40),(0,41,41),(0,42,42),(0,43,43),(0,44,44),(0,45,45),(0,46,46),(0,47,47),(0,48,48),(0,49,49),(0,50,50),(0,51,51),(0,52,52),(0,53,53),
               (0,54,54),(0,55,55),(0,56,56),(0,57,57),(0,58,58),(0,59,59),(0,60,60),(0,61,61),(0,62,62),(0,63,63),(0,64,64),(0,65,65),(0,66,66),(0,67,67),(0,68,68),(0,69,69),(0,70,70),(0,71,71),
               (0,72,72),(0,73,73),(0,74,74),(0,75,75),(0,76,76),(0,77,77),(0,78,78),(0,79,79),(0,80,80),(0,81,81),(0,82,82),(0,83,83),(0,84,84),(0,85,85),(0,86,86),(0,87,87),(0,88,88),(0,89,89),
               (0,90,90),(0,91,91),(0,92,92),(0,93,93),(0,94,94),(0,95,95),(0,96,96),(0,97,97),(0,98,98),(0,99,99),(0,100,100),(1,0,1),(2,0,2),(3,0,3),(3,4,5),(4,0,4),(4,3,5),(5,0,5),(5,12,13),
               (6,0,6),(6,8,10),(7,0,7),(7,24,25),(8,0,8),(8,6,10),(8,15,17),(9,0,9),(9,12,15),(9,40,41),(10,0,10),(10,24,26),(11,0,11),(11,60,61),(12,0,12),(12,5,13),(12,9,15),(12,16,20),(12,35,37),
               (13,0,13),(13,84,85),(14,0,14),(14,48,50),(15,0,15),(15,8,17),(15,20,25),(15,36,39),(16,0,16),(16,12,20),(16,30,34),(16,63,65),(17,0,17),(18,0,18),(18,24,30),(18,80,82),(19,0,19),
               (20,0,20),(20,15,25),(20,21,29),(20,48,52),(21,0,21),(21,20,29),(21,28,35),(21,72,75),(22,0,22),(23,0,23),(24,0,24),(24,7,25),(24,10,26),(24,18,30),(24,32,40),(24,45,51),(24,70,74),
               (25,0,25),(25,60,65),(26,0,26),(27,0,27),(27,36,45),(28,0,28),(28,21,35),(28,45,53),(28,96,100),(29,0,29),(30,0,30),(30,16,34),(30,40,50),(30,72,78),(31,0,31),(32,0,32),(32,24,40),
               (32,60,68),(33,0,33),(33,44,55),(33,56,65),(34,0,34),(35,0,35),(35,12,37),(35,84,91),(36,0,36),(36,15,39),(36,27,45),(36,48,60),(36,77,85),(37,0,37),(38,0,38),(39,0,39),(39,52,65),
               (39,80,89),(40,0,40),(40,9,41),(40,30,50),(40,42,58),(40,75,85),(41,0,41),(42,0,42),(42,40,58),(42,56,70),(43,0,43),(44,0,44),(44,33,55),(45,0,45),(45,24,51),(45,28,53),(45,60,75),
               (46,0,46),(47,0,47),(48,0,48),(48,14,50),(48,20,52),(48,36,60),(48,55,73),(48,64,80),(49,0,49),(50,0,50),(51,0,51),(51,68,85),(52,0,52),(52,39,65),(53,0,53),(54,0,54),(54,72,90),
               (55,0,55),(55,48,73),(56,0,56),(56,33,65),(56,42,70),(57,0,57),(57,76,95),(58,0,58),(59,0,59),(60,0,60),(60,11,61),(60,25,65),(60,32,68),(60,45,75),(60,63,87),(60,80,100),(61,0,61),
               (62,0,62),(63,0,63),(63,16,65),(63,60,87),(64,0,64),(64,48,80),(65,0,65),(65,72,97),(66,0,66),(67,0,67),(68,0,68),(68,51,85),(69,0,69),(70,0,70),(70,24,74),(71,0,71),(72,0,72),
               (72,21,75),(72,30,78),(72,54,90),(72,65,97),(73,0,73),(74,0,74),(75,0,75),(75,40,85),(76,0,76),(76,57,95),(77,0,77),(77,36,85),(78,0,78),(79,0,79),(80,0,80),(80,18,82),(80,39,89),
               (80,60,100),(81,0,81),(82,0,82),(83,0,83),(84,0,84),(84,13,85),(84,35,91),(85,0,85),(86,0,86),(87,0,87),(88,0,88),(89,0,89),(90,0,90),(91,0,91),(92,0,92),(93,0,93),(94,0,94),(95,0,95),
               (96,0,96),(96,28,100),(97,0,97),(98,0,98), (100,0,100), (99,0,99)] == True
          then putStrLn "Works! This test is also accounting for the possibility that you may be providing lists that are in a different order than mine. Check to make sure you did not use recursion. WARNING: BUGGY"
          else putStrLn "Does not work.\n This test is using the class to test my own code here.\nIf this test is failing, please notify me, your code may or may not be wrong.\nYou also must include a (0,0,0) base case for this test to work."

          putStr "6. pushRight: "
          if   pushRight "crocodile" 9 == "crocodile" &&
               pushRight "crocodile" 12 == "   crocodile" &&
               pushRight "crocodile" 40 == "                               crocodile" &&
               pushRight "" 0 == "" &&
               pushRight "" 3 == "   "
          then putStrLn "Works! Check to make sure that you used list library functions or list comprehension and not recursion."
          else putStrLn "Does not work."

          putStr "7. lowerFirstCharacter: "
          if   lowerFirstCharacter "Crocodile" == "crocodile" &&
               lowerFirstCharacter " " == " " &&
               lowerFirstCharacter "" == "" &&
               lowerFirstCharacter "D" == "d" &&
               lowerFirstCharacter " D" == " D" &&
               lowerFirstCharacter " *^~ C *^~ c *^~ D *^~ E *^~" == " *^~ C *^~ c *^~ D *^~ E *^~" &&
               lowerFirstCharacter "E *^~ C *^~ c *^~ D *^~ E *^~" == "e *^~ C *^~ c *^~ D *^~ E *^~" &&
               lowerFirstCharacter "+^Cro+^codile" == "+^Cro+^codile" && -- Remove if he's not including non-alphabetic characters, I am still waiting to hear back on Slack about this.
               lowerFirstCharacter "crocodile" == "crocodile"
          then putStrLn "Works! Check to make sure that you used list library functions or list comprehension and not recursion."
          else putStrLn "Does not work."

          putStr "8. middleWord: "
          if   middleWord "Haskell is fun!" == "is" &&
               middleWord "#&@* 223* ((3!" == "223*"
          then putStrLn "Works! Check to make sure that you used list library functions or list comprehension and not recursion."
          else putStrLn "Does not work."

          putStr "9. lowerFirstLetter: "
          if   lowerFirstLetter "crocodile" == "crocodile" &&
               lowerFirstLetter " " == " " &&
               lowerFirstLetter "" == "" &&
               lowerFirstLetter " *^~ C *^~ c *^~ D *^~ E *^~" == " *^~ c *^~ c *^~ D *^~ E *^~" &&
               lowerFirstLetter "+^crOc+^oDile+^" == "+^croc+^oDile+^" && -- Remove if he's not including non-alphabetic characters, I am still waiting to hear back on Slack about this.
               lowerFirstLetter "cRoCodilE" == "croCodilE"
          then putStrLn "Works! Check to make sure that you used list library functions or list comprehension and not recursion."
          else putStrLn "Does not work."

          putStr "10. lowerFirstTwoLetters: "
          if   lowerFirstTwoLetters "CRocOdilE" == "crocOdilE" &&
               lowerFirstTwoLetters " " == " " &&
               lowerFirstTwoLetters " *^~ C *^~ c *^~ D *^~ E *^~" == " *^~ c *^~ c *^~ d *^~ E *^~" &&
               lowerFirstTwoLetters "+^crOc+^oDilE+^" == "+^croc+^odilE+^" && -- Remove if he's not including non-alphabetic characters, I am still waiting to hear back on Slack about this.
               lowerFirstTwoLetters "cRoCodilE" == "crocodilE" &&
               lowerFirstTwoLetters "cRo+^coDilE" == "cro+^codilE"
          then putStrLn "Works! Check to make sure that you used list library functions or list comprehension and not recursion."
          else putStrLn "Does not work."