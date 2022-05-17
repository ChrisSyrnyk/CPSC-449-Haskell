module ErrorCheck where

--functions for checking for invalid penalty:
import System.Exit
import Data.Char (ord)
import OutputFileMaker

--Just adding some driving code - Abhay
getTripletFirst :: (a,b,c) -> a
getTripletFirst (x,_,_) = x

getTripletSecond :: (a,b,c) -> b
getTripletSecond (_,y,_) = y
-----

--Filter out positive values from machinePenalties [[Int]] and return [Int]
--Will be used to determine if any negative penalty values have been input
--Used [Int] output as input in invalidPenaltyCheck
machinePenaltiesFilter :: Int -> [[Int]] -> [Int]
--Always set i to 0, Always set j to 1
--xs is the input [[Int]]
machinePenaltiesFilter i [xs] = [0]
machinePenaltiesFilter i xs =
  if i<(length xs -1) then filter (<0) (xs!!i) ++ machinePenaltiesFilter (i+1) xs
  else filter (<0) (xs!!i)

--takes input from machinePenaltiesFilter. If the length is >0 then negative values have been input
--returns 2 if negative values have been input. 0 if not (to be used as input in errorCheck)
invalidPenaltyCheck :: [Int] -> Int
invalidPenaltyCheck xs =
  if length xs > 0 then 2
  else 0

--functions for checking for invalid task:

--can get ascii value of character by using (ord 'a'). *********** need to import -> Data.Char (ord) ***************
--from ascii table A==65 B==66 ... H==72
--Should be able to filer by ascii v+alues
tasks = ['A','B','C','D','E','F','G','a']
ascii = charToInt 0 tasks
returnValue = invalidTaskError ascii

--convert char array to ascii values
--used [Int] out put as input for invalidTaskError
charToInt :: Int -> [Char] -> [Int]
--set i to 0. xs to char array of tasks
charToInt i xs =
  if i<length xs -1 then ord (xs!!i) : charToInt (i+1) xs
  else [ord (xs!!i)]

 --check ascii values fall in range [A->H]. return error 3 if outside range(to be used as input in errorCheck)
invalidTaskError :: [Int] -> Int
invalidTaskError xs =
  if length (filter (>72) xs) > 0 || length (filter (<65) xs) >0 then 3
  else 0

-- This method is to get the whole TooNearPenalty triplet list and find all the tasks in that 
-- Returns a list containing all the tasks inside the tooNearPenalty triplet list. 
invalidTaskChecker :: [(Char, Char, Int)] -> [Char]
invalidTaskChecker [] = "A"
invalidTaskChecker [x] = 
    [(getTripletFirst x)] ++ [(getTripletSecond x)]
invalidTaskChecker (x:xs) = 
    [(getTripletFirst x)] ++ [(getTripletSecond x)] ++ (invalidTaskChecker xs) 

--from ascii table A==65 B==66 ... H==72
--return error value 4 if a task or machine is out of range
invalidMachineTaskCheck :: Int -> [(Int,Char)] ->Int
--Always set i to 0 
invalidMachineTaskCheck i [] = 0
invalidMachineTaskCheck i xs
  | i >= length xs = 0
  | ord(snd(xs!!i)) > 72 || ord(snd(xs!!i)) <65 || fst(xs!!i) < 1 || fst(xs!!i) >8 = 4
  | otherwise = invalidMachineTaskCheck (i+1) xs

invalidTooNearCheck :: Int -> [(Char, Char)] -> Int
--Always set i to 0
invalidTooNearCheck i [] = 0
invalidTooNearCheck i xs
  | i >= length xs = 0
  | ord(snd(xs!!i)) > 72 || ord(snd(xs!!i)) <65 || ord(fst(xs!!i)) > 72 || ord(fst(xs!!i)) <65 = 4
  | otherwise = invalidTooNearCheck (i+1) xs

--function for terminating program and outputing respective error message
--
errorerrorCheck :: Int -> String -> IO()
--x is the error value 
errorerrorCheck x fileName
  | x == 1 = do
    --putStr "partial assignment error"
    outputWriteErrorMsg "partial assignment error" fileName
    exitSuccess
  | x== 2 = do
     --putStr "invalid penalty"
     outputWriteErrorMsg "invalid penalty" fileName
     exitSuccess
  | x== 3 = do
    --putStr "invalid task"
    outputWriteErrorMsg "invalid task" fileName
    exitSuccess
  | x== 4 = do
    --putStr "invalid machine/task"
    outputWriteErrorMsg "invalid machine/task" fileName
    exitSuccess
  | otherwise = return()
