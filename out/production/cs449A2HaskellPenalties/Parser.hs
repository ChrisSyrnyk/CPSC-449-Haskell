module Parser where

import System.Environment ()
import System.IO ()
import System.IO.Error ()
import Data.List (delete)
--import Data.Typeable
--import System.Directory
import Penalties

--import

{-
INT = 0
ALPHA_CHAR = 1
OPEN_BRACKET = 40
CLOSED_BRACKET = 41
COMMA = 44

INT_ALPHA_CHAR_PAIR_SYNTAX_SEQ = [OPEN_BRACKET, INT, COMMA, ALPHA_CHAR, CLOSED_BRACKET]

ascii0 :: Int
ascii0 = 48
ascii9 :: Int
ascii9 = 57
asciiCapA :: Int
asciiCapA = 65
asciiCapH :: Int
asciiCapH = 72
-}
defaultIntCharPair :: (Int, Char)
defaultIntCharPair = (-1, ' ')
defaultCharCharPair :: (Char, Char)
defaultCharCharPair = (' ', ' ')
defaultCharCharIntTriplet :: (Char,Char,Int)
defaultCharCharIntTriplet = (' ',' ',-1)

constraintNames :: [String]
constraintNames = ["forced partial assignment:","forbidden machine:","too-near tasks:","machine penalties:","too-near penalities:"]
forcedPartialAssigsStr :: String
forcedPartialAssigsStr = constraintNames !! 0
forbiddenMachinesStr :: String
forbiddenMachinesStr = constraintNames !! 1
tooNearTasksStr :: String
tooNearTasksStr = constraintNames !! 2
machinePenaltiesStr :: String
machinePenaltiesStr = constraintNames !! 3
tooNearPenaltiesStr :: String
tooNearPenaltiesStr = constraintNames !! 4

constraintInputInvalid :: Int
constraintInputInvalid = -1

forcedPartialAssigNumOfElements :: Int
forcedPartialAssigNumOfElements = (length forcedPartialAssigUpperAndLowerBounds) `div` 2
forcedPartialAssigUpperAndLowerBounds :: [Char]
forcedPartialAssigUpperAndLowerBounds = ['0','9','A','H']

forbiddenMachinesNumOfElements :: Int
forbiddenMachinesNumOfElements = (length forbiddenMachinesUpperAndLowerBounds) `div` 2
forbiddenMachinesUpperAndLowerBounds :: [Char]
forbiddenMachinesUpperAndLowerBounds = ['0','9','A','H']

tooNearTasksNumOfElements :: Int
tooNearTasksNumOfElements = (length tooNearTasksUpperAndLowerBounds) `div` 2
tooNearTasksUpperAndLowerBounds :: [Char]
tooNearTasksUpperAndLowerBounds = ['A','H','A','H']

machinePenaltiesRowsAndCols :: Int
machinePenaltiesRowsAndCols = 8
machinePenaltiesUpperAndLowerBounds :: [Char]
machinePenaltiesUpperAndLowerBounds = ['0','9']
{- Old version that allowed bound to be set for the first character of each element in 2D array. Ended up giving me "cannot construct infinite type" error
machinePenaltiesCols :: Int
machinePenaltiesCols = (length (machinePenaltiesUpperAndLowerBounds !! 0)) `div` 2
machinePenaltiesUpperAndLowerBounds :: [[Char]]
machinePenaltiesUpperAndLowerBounds = [['0','9','0','9','0','9','0','9','0','9','0','9','0','9','0','9','0','9','0','9'],['0','9','0','9','0','9','0','9','0','9','0','9','0','9','0','9','0','9','0','9'],['0','9','0','9','0','9','0','9','0','9','0','9','0','9','0','9','0','9','0','9'],['0','9','0','9','0','9','0','9','0','9','0','9','0','9','0','9','0','9','0','9'],['0','9','0','9','0','9','0','9','0','9','0','9','0','9','0','9','0','9','0','9'],['0','9','0','9','0','9','0','9','0','9','0','9','0','9','0','9','0','9','0','9'],['0','9','0','9','0','9','0','9','0','9','0','9','0','9','0','9','0','9','0','9'],['0','9','0','9','0','9','0','9','0','9','0','9','0','9','0','9','0','9','0','9'],['0','9','0','9','0','9','0','9','0','9','0','9','0','9','0','9','0','9','0','9'],['0','9','0','9','0','9','0','9','0','9','0','9','0','9','0','9','0','9','0','9'],['0','9','0','9','0','9','0','9','0','9','0','9','0','9','0','9','0','9','0','9']]
-}
tooNearPenaltiesNumOfElements :: Int
tooNearPenaltiesNumOfElements = (length tooNearPenaltiesUpperAndLowerBounds) `div` 2
tooNearPenaltiesUpperAndLowerBounds :: [Char]
tooNearPenaltiesUpperAndLowerBounds = ['A','H','A','H','0','9']




data ConstraintsFound = ConstraintsFound { forcedPartialAssigsFound :: Bool
                                         , forbiddenMachinesFound :: Bool
                                         , tooNearTasksFound :: Bool
                                         , machinePenaltiesFound :: Bool
                                         , tooNearPenaltiesFound :: Bool
                                         } deriving (Show)

parseFileHandleExceptions :: String -> IO()
parseFileHandleExceptions filePath = do
  fileContents <- readFile filePath
  let fileLines = lines fileContents
      penalties = returnConstraints fileLines 0 constraintNames (Penalties [(9, 'Z')] [(9, 'Z')] [('Z', 'Z')] [[9,9],[9,9]] [('Z', 'Z', 9)])
  print penalties
  ---inputFileHandle <- openFile filePath ReadMode
  ---stringToPrint <- readUntilNotWhitespaceRead inputFileHandle ""
  --let inputFileHandle = openInputFile filePath ReadMode
  --putStrLn "hi"

  --return ()

returnConstraints :: [String] -> Int -> [String] -> Penalties -> Penalties
returnConstraints fileLines currLineNum constraintNamesLeft currPenalties
  | currLineNum == (length fileLines) - 1 = currPenalties --when end of file is reached
  | currLine == forcedPartialAssigsStr    = if (lineNumWhenForcedPartialAssigsInputEnds /= constraintInputInvalid) then returnConstraints fileLines lineNumWhenForcedPartialAssigsInputEnds constraintNamesLeftRemoveForcedPartialAssigs (Penalties forcedPartialAssigsList (forbiddenMachines currPenalties) (tooNearTasks currPenalties) (machinePenalties currPenalties) (tooNearPenalties currPenalties)) else currPenalties
  | currLine == forbiddenMachinesStr      = if (lineNumWhenForbiddenMachinesInputEnds /= constraintInputInvalid) then returnConstraints fileLines lineNumWhenForbiddenMachinesInputEnds constraintNamesLeftRemoveForbiddenMachines (Penalties (forcedPartialAssigs currPenalties) forbiddenMachinesList (tooNearTasks currPenalties) (machinePenalties currPenalties) (tooNearPenalties currPenalties)) else currPenalties
  | currLine == tooNearTasksStr           = if (lineNumWhenTooNearTasksInputEnds /= constraintInputInvalid) then returnConstraints fileLines lineNumWhenTooNearTasksInputEnds constraintNamesLeftRemoveTooNearTasks (Penalties (forcedPartialAssigs currPenalties) (forbiddenMachines currPenalties) tooNearTasksList (machinePenalties currPenalties) (tooNearPenalties currPenalties)) else currPenalties
  | currLine == machinePenaltiesStr       = if (lineNumWhenMachinePenaltiesInputEndsStored /= constraintInputInvalid) then returnConstraints fileLines lineNumWhenMachinePenaltiesInputEndsStored constraintNamesLeftRemoveMachinePenalties (Penalties (forcedPartialAssigs currPenalties) (forbiddenMachines currPenalties) (tooNearTasks currPenalties) machinePenaltiesList (tooNearPenalties currPenalties)) else currPenalties
  | currLine == tooNearPenaltiesStr       = if (lineNumWhenTooNearPenaltiesInputEnds /= constraintInputInvalid) then returnConstraints fileLines lineNumWhenTooNearPenaltiesInputEnds constraintNamesLeftRemoveTooNearPenalties (Penalties (forcedPartialAssigs currPenalties) (forbiddenMachines currPenalties) (tooNearTasks currPenalties) (machinePenalties currPenalties) tooNearPenaltiesList) else currPenalties
  | otherwise                             = returnConstraints fileLines (currLineNum + 1) constraintNamesLeft currPenalties
  where currLine = fileLines !! currLineNum
        nextLineNum = currLineNum + 1
        lineNumWhenForcedPartialAssigsInputEnds = lineNumWhenConstraintInputEnds fileLines nextLineNum constraintNamesLeftRemoveForcedPartialAssigs forcedPartialAssigNumOfElements forcedPartialAssigUpperAndLowerBounds
        lineNumWhenForbiddenMachinesInputEnds = lineNumWhenConstraintInputEnds fileLines nextLineNum constraintNamesLeftRemoveForbiddenMachines forbiddenMachinesNumOfElements forbiddenMachinesUpperAndLowerBounds
        lineNumWhenTooNearTasksInputEnds = lineNumWhenConstraintInputEnds fileLines nextLineNum constraintNamesLeftRemoveTooNearTasks tooNearTasksNumOfElements tooNearTasksUpperAndLowerBounds
        lineNumWhenMachinePenaltiesInputEndsStored = lineNumWhenMachinePenaltiesInputEnds fileLines nextLineNum 0 constraintNamesLeftRemoveMachinePenalties machinePenaltiesRowsAndCols machinePenaltiesUpperAndLowerBounds ' '
        lineNumWhenTooNearPenaltiesInputEnds = lineNumWhenConstraintInputEnds fileLines nextLineNum constraintNamesLeftRemoveTooNearPenalties tooNearPenaltiesNumOfElements tooNearPenaltiesUpperAndLowerBounds
        constraintsIntCharPairList = getPairListFromStrs fileLines nextLineNum getIntCharPairFromStr defaultIntCharPair []
        constraintsCharCharPairList = getPairListFromStrs fileLines nextLineNum getCharCharPairFromStr defaultCharCharPair []
        constraintsCharCharIntTripletList = getTripletListFromStrs fileLines nextLineNum getTripletFromStr defaultCharCharIntTriplet []
        forcedPartialAssigsList = constraintsIntCharPairList
        forbiddenMachinesList = constraintsIntCharPairList
        tooNearTasksList = constraintsCharCharPairList
        machinePenaltiesList = get2DArrayFromStrs fileLines nextLineNum machinePenaltiesRowsAndCols ' ' getIntListFromStr []
        tooNearPenaltiesList = constraintsCharCharIntTripletList
        constraintNamesLeftRemoveForcedPartialAssigs = delete forcedPartialAssigsStr constraintNamesLeft
        constraintNamesLeftRemoveForbiddenMachines = delete forbiddenMachinesStr constraintNamesLeft
        constraintNamesLeftRemoveTooNearTasks = delete tooNearTasksStr constraintNamesLeft
        constraintNamesLeftRemoveMachinePenalties = delete machinePenaltiesStr constraintNamesLeft
        constraintNamesLeftRemoveTooNearPenalties = delete tooNearPenaltiesStr constraintNamesLeft

lineNumWhenConstraintInputEnds :: [String] -> Int -> [String] -> Int -> [Char] -> Int
lineNumWhenConstraintInputEnds fileLines currLineNum constraintNamesLeft constraintNumOfElements constraintUpperAndLowerBounds
  | currLineNum == (length fileLines) - 1                                                            = currLineNum --when end of file is reached
  | currLine == "" || (all (== ' ') currLine)                                                        = checkNextLine
  | isStringConstraintSyntaxValid currLine 0 0 constraintNumOfElements constraintUpperAndLowerBounds = checkNextLine
  | currLine `elem` constraintNamesLeft                                                              = currLineNum
  | otherwise                                                                                        = constraintInputInvalid
  where currLine = fileLines !! currLineNum
        checkNextLine = lineNumWhenConstraintInputEnds fileLines (currLineNum + 1) constraintNamesLeft constraintNumOfElements constraintUpperAndLowerBounds

isStringConstraintSyntaxValid :: [Char] -> Int -> Int -> Int -> [Char] -> Bool
isStringConstraintSyntaxValid line currCharIdx currSeqIdx elements elemLowerAndUpperBounds --Bounds are ASCII Codes
  | currSeqIdx == 0                                                        = isCurrCharThisCharIfSoCheckRemainingChars '('
  | currSeqAnElement && (currSeqLowerBoundAnInt && currSeqUpperBoundAnInt) = isCurrCharBetweenTheseCharsAndAnIntIfSoCheckRemainingCharsAfterInt currSeqLowerBound currSeqUpperBound
  | currSeqAnElement                                                       = isCurrCharBetweenTheseCharsIfSoCheckRemainingChars currSeqLowerBound currSeqUpperBound
  | currSeqIdx `elem` (take (elements - 1) [2,4..])                        = isCurrCharThisCharIfSoCheckRemainingChars ','
  | currSeqIdx == elements * 2                                             = isCurrCharThisChar ')'
  | otherwise                                                              = False --this condition should never happen as elements should not let currSeqIdx to get bigger than it should be
  where currChar = (line !! currCharIdx)
        currSeqAnElement = currSeqIdx `elem` (take elements [1,3..])
        currSeqLowerBound = elemLowerAndUpperBounds !! (currSeqIdx - 1)
        currSeqUpperBound = elemLowerAndUpperBounds !! currSeqIdx
        currSeqLowerBoundAnInt = currSeqLowerBound >= '0' && currSeqLowerBound <= '9'
        currSeqUpperBoundAnInt = currSeqUpperBound >= '0' && currSeqUpperBound <= '9'
        isCurrCharThisCharIfSoCheckRemainingChars :: Char -> Bool
        isCurrCharThisCharIfSoCheckRemainingChars charShouldBeThisChar = if currChar == charShouldBeThisChar then True && remainingCharsValid else False
        isCurrCharThisChar :: Char -> Bool
        isCurrCharThisChar charShouldBeThisChar = currChar == charShouldBeThisChar
        isCurrCharBetweenTheseCharsIfSoCheckRemainingChars :: Char -> Char -> Bool
        isCurrCharBetweenTheseCharsIfSoCheckRemainingChars lowerBound upperBound = if currChar >= lowerBound && currChar <= upperBound then True && remainingCharsValid else False --False here used to be thisCharAndRemainingCharsValid but I think False is correct here
        isCurrCharBetweenTheseCharsAndAnIntIfSoCheckRemainingCharsAfterInt :: Char -> Char -> Bool
        isCurrCharBetweenTheseCharsAndAnIntIfSoCheckRemainingCharsAfterInt lowerBound upperBound = if currChar >= lowerBound && currChar <= upperBound then True && remainingCharsAfterIntValid else False --False here used to be thisCharAndRemainingCharsValid but I think False is correct here
        remainingCharsValid = isStringConstraintSyntaxValid line (currCharIdx + 1) (currSeqIdx + 1) elements elemLowerAndUpperBounds
        remainingCharsAfterIntValid = isStringConstraintSyntaxValid line nextCharIdxAfterInt (currSeqIdx + 1) elements elemLowerAndUpperBounds
        --thisCharAndRemainingCharsValid = isStringConstraintSyntaxValid line currCharIdx (currSeqIdx - 1) elements elemLowerAndUpperBounds --currently not needed as I believe using False in the else statement of isCurrCharBetweenTheseCharsIfSoCheckRemainingChars and isCurrCharBetweenTheseCharsAndAnIntIfSoCheckRemainingCharsAfterInt is correct. This is because I originally did this to account for ints so if you ran into a ')' or ',' and you weren't finished reading the int, it would go back in sequence (so that it would expect and int) but stay on the same char. This implementation does not work as this would allow two AAs to be inputted in a row when you only want one A. Thus, I used a different implementation.
        nextCharIdxAfterInt = currCharIdx + (length $ show intInPair)
        intInPair = getIntFromStr (drop currCharIdx line) 0

getPairListFromStrs :: [String] -> Int -> (String -> Int -> Int -> (a,b) -> (a,b)) -> (a,b) -> [(a,b)] -> [(a,b)]
getPairListFromStrs fileLines currLineNum getPairFromStrFunc defaultPair currPairs
  | currLineNum == (length fileLines) - 1     = currPairs --when end of file is reached
  | currLine `elem` constraintNames           = currPairs
  | currLine == "" || (all (== ' ') currLine) = checkNextLine currPairs
  | otherwise                                 = checkNextLine (currPairs ++ [getPairFromStrFunc currLine 0 0 defaultPair])
  where currLine = fileLines !! currLineNum
        nextLineNum = currLineNum + 1
        checkNextLine = getPairListFromStrs fileLines nextLineNum getPairFromStrFunc defaultPair

getIntCharPairFromStr :: String -> Int -> Int -> (Int,Char) -> (Int,Char)
getIntCharPairFromStr line currCharIdx currSeqIdx currIntCharPair --Bounds are ASCII Codes
  | currSeqIdx == 0 = getIntCharPairFromStr line nextCharIdx nextSeqIdx currIntCharPair --when currChar is '('
  | currSeqIdx == 1 = getIntCharPairFromStr line nextCharIdxAfterInt nextSeqIdx (intInPair, ' ') --when currChar is char in int input (first element in pair)
  | currSeqIdx == 2 = getIntCharPairFromStr line nextCharIdx nextSeqIdx currIntCharPair --when currChar is ','
  | currSeqIdx == 3 = (fst currIntCharPair, currChar) --when currChar is char in char input (second element in pair)
  | otherwise       = currIntCharPair --this condition should never happen as currSeqIdx should not get bigger than 3 (recursion should stop)
  where currChar = line !! currCharIdx
        nextCharIdx = currCharIdx + 1
        nextSeqIdx = currSeqIdx + 1
        nextCharIdxAfterInt = currCharIdx + (length $ show intInPair)
        intInPair = getIntFromStr (drop currCharIdx line) 0

getCharCharPairFromStr :: String -> Int -> Int -> (Char,Char) -> (Char,Char)
getCharCharPairFromStr line currCharIdx currSeqIdx currCharCharPair --Bounds are ASCII Codes
  | currSeqIdx == 0 = getCharCharPairFromStr line nextCharIdx nextSeqIdx currCharCharPair --when currChar is '('
  | currSeqIdx == 1 = getCharCharPairFromStr line nextCharIdx nextSeqIdx (currChar, snd currCharCharPair) --when currChar is char in char input (first element in pair)
  | currSeqIdx == 2 = getCharCharPairFromStr line nextCharIdx nextSeqIdx currCharCharPair --when currChar is ','
  | currSeqIdx == 3 = (fst currCharCharPair, currChar) --when currChar is char in char input (second element in pair)
  | otherwise       = currCharCharPair --this condition should never happen as currSeqIdx should not get bigger than 3 (recursion should stop)
  where currChar = line !! currCharIdx
        nextCharIdx = currCharIdx + 1
        nextSeqIdx = currSeqIdx + 1

getTripletListFromStrs :: [String] -> Int -> (String -> Int -> Int -> (a,b,c) -> (a,b,c)) -> (a,b,c) -> [(a,b,c)] -> [(a,b,c)]
getTripletListFromStrs fileLines currLineNum getTripletFromStrFunc defaultTriplet currTriplets
 | currLineNum == (length fileLines) - 1     = currTriplets --when end of file is reached
 | currLine `elem` constraintNames           = currTriplets
 | currLine == "" || (all (== ' ') currLine) = checkNextLine currTriplets
 | otherwise                                 = checkNextLine (currTriplets ++ [getTripletFromStrFunc currLine 0 0 defaultTriplet])
 where currLine = fileLines !! currLineNum
       nextLineNum = currLineNum + 1
       checkNextLine = getTripletListFromStrs fileLines nextLineNum getTripletFromStrFunc defaultTriplet

getTripletFromStr ::  String -> Int -> Int -> (Char,Char,Int) -> (Char,Char,Int) --currently the Triplet can only be (Char,Char,Int)
getTripletFromStr line currCharIdx currSeqIdx currTriplet --Bounds are ASCII Codes
 | currSeqIdx == 0                    = getTripletFromStr line nextCharIdx nextSeqIdx currTriplet --when currChar is '('
 | currSeqIdx == 1                    = getTripletFromStr line nextCharIdx nextSeqIdx (currChar, getTripletSecond currTriplet, getTripletThird currTriplet) --when currChar is char in char input (first element in triplet)
 | currSeqIdx == 2 || currSeqIdx == 4 = getTripletFromStr line nextCharIdx nextSeqIdx currTriplet --when currChar is ','
 | currSeqIdx == 3                    = getTripletFromStr line nextCharIdx nextSeqIdx (getTripletFirst currTriplet, currChar, getTripletThird currTriplet)  --when currChar is char in char input (second element in triplet)
 | currSeqIdx == 5                    = (getTripletFirst currTriplet, getTripletSecond currTriplet, intInPair)  --when currChar is char in char input (second element in triplet)
 | otherwise                          = currTriplet --this condition should never happen as currSeqIdx should not get bigger than 3 (recursion should stop)
 where currChar = line !! currCharIdx
       nextCharIdx = currCharIdx + 1
       nextSeqIdx = currSeqIdx + 1
       intInPair = getIntFromStr (drop currCharIdx line) 0
       --nextCharIdxAfterInt = currCharIdx + (length $ show intInPair) --to be used when I make this func more generic ie. doesn't have to read (Char,Char,Int)

getIntFromStr :: String -> Int -> Int --Don't know if this func works if int in string ends with empty character. For our program though, it only requires us to have either a ')' or a ',' to signify the end of the int
getIntFromStr line currCharIdx --line here must start with int char or otherwise read func will try and read the empty list and I think an error will occur
  | currCharIsInt && not isLastCharInLine = getIntFromStr line nextCharIdx
  | currCharIsInt && isLastCharInLine     = read $ take (currCharIdx + 1) line 
  | otherwise                             = read $ take currCharIdx line
  where currChar = line !! currCharIdx
        nextCharIdx = currCharIdx + 1
        currCharIsInt = currChar >= '0' && currChar <= '9'
        isLastCharInLine = currCharIdx == ((length line) - 1)

getTripletFirst :: (a,b,c) -> a
getTripletFirst (x,_,_) = x

getTripletSecond :: (a,b,c) -> b
getTripletSecond (_,y,_) = y

getTripletThird :: (a,b,c) -> c
getTripletThird (_,_,z) = z

lineNumWhenMachinePenaltiesInputEnds :: [String] -> Int -> Int -> [String] -> Int -> [Char] -> Char -> Int
lineNumWhenMachinePenaltiesInputEnds fileLines currLineNum currRowNum constraintNamesLeft cols constraintUpperAndLowerBounds demarcationChar
  | currLineNum == (length fileLines) - 1                            = currLineNum --when end of file is reached
  | currLine == "" || (all (== ' ') currLine)                        = checkNextLineLineWhitespace
  | lineValid  && notAllRowsHaveBeenReadYet                          = checkNextLineLineValid
  | currLine `elem` constraintNamesLeft                              = currLineNum
  | otherwise                                                        = constraintInputInvalid
  where currLine = fileLines !! currLineNum
        nextCurrRowNum = currRowNum + 1
        checkNextLineLineWhitespace = lineNumWhenMachinePenaltiesInputEnds fileLines (currLineNum + 1) currRowNum constraintNamesLeft cols constraintUpperAndLowerBounds demarcationChar
        checkNextLineLineValid = lineNumWhenMachinePenaltiesInputEnds fileLines (currLineNum + 1) nextCurrRowNum constraintNamesLeft cols constraintUpperAndLowerBounds demarcationChar
        lineValid = isStringMachinePenaltiesSyntaxValid currLine 0 0 cols constraintUpperAndLowerBounds demarcationChar
        notAllRowsHaveBeenReadYet = currRowNum /= machinePenaltiesRowsAndCols

isStringMachinePenaltiesSyntaxValid :: [Char] -> Int -> Int -> Int -> [Char] -> Char -> Bool
isStringMachinePenaltiesSyntaxValid line currCharIdx currSeqIdx cols elemLowerAndUpperBounds demarcationChar
 | currSeqIdx == (cols * 2) - 2                                                    = isCurrCharBetweenTheseCharsAndAnIntAndTheLastCharsOfLine currCharLowerBound currCharUpperBound
 | currSeqIdx `mod` 2 == 0 && (currCharLowerBoundAnInt && currCharUpperBoundAnInt) = isCurrCharBetweenTheseCharsAndAnIntIfSoCheckRemainingCharsAfterInt currCharLowerBound currCharUpperBound
{-
 | currSeqIdx `mod` 2 == 0 if you want to account for case that entries can be letters and not numbers
-}
 | currSeqIdx `mod` 2 == 1                                                         = isCurrCharThisCharIfSoCheckRemainingChars demarcationChar
 | otherwise                                                                       = False --this condition should never happen as elements should not let currCharIdx to get bigger than it should be (first guard is the last recursive call)
 where currChar = (line !! currCharIdx)
       nextCharIdx = currCharIdx + 1
       nextSeqIdx = currSeqIdx + 1
       currCharLowerBound = elemLowerAndUpperBounds !! 0
       currCharUpperBound = elemLowerAndUpperBounds !! 1
       currCharLowerBoundAnInt = currCharLowerBound >= '0' && currCharLowerBound <= '9'
       currCharUpperBoundAnInt = currCharUpperBound >= '0' && currCharUpperBound <= '9'
       isCurrCharThisCharIfSoCheckRemainingChars :: Char -> Bool
       isCurrCharThisCharIfSoCheckRemainingChars charShouldBeThisChar = if currChar == charShouldBeThisChar then True && remainingCharsValid else False
       --isCurrCharThisChar :: Char -> Bool
       --isCurrCharThisChar charShouldBeThisChar = if currChar == charShouldBeThisChar then True else False
       --isCurrCharBetweenTheseCharsIfSoCheckRemainingChars :: Char -> Char -> Bool
       --isCurrCharBetweenTheseCharsIfSoCheckRemainingChars lowerBound upperBound = if currChar >= lowerBound && currChar <= upperBound then True && remainingCharsValid else False --False here used to be thisCharAndRemainingCharsValid but I think False is correct here
       isCurrCharBetweenTheseCharsAndAnIntIfSoCheckRemainingCharsAfterInt :: Char -> Char -> Bool
       isCurrCharBetweenTheseCharsAndAnIntIfSoCheckRemainingCharsAfterInt lowerBound upperBound = if currChar >= lowerBound && currChar <= upperBound then True && remainingCharsAfterIntValid else False --False here used to be thisCharAndRemainingCharsValid but I think False is correct here
       isCurrCharBetweenTheseCharsAndAnIntAndTheLastCharsOfLine :: Char -> Char -> Bool
       isCurrCharBetweenTheseCharsAndAnIntAndTheLastCharsOfLine lowerBound upperBound = (currChar >= lowerBound) && (currChar <= upperBound) && (nextCharIdxAfterInt == (length line))
       remainingCharsValid = isStringMachinePenaltiesSyntaxValid line nextCharIdx nextSeqIdx cols elemLowerAndUpperBounds demarcationChar
       remainingCharsAfterIntValid = isStringMachinePenaltiesSyntaxValid line nextCharIdxAfterInt nextSeqIdx cols elemLowerAndUpperBounds demarcationChar
       --thisCharAndRemainingCharsValid = isStringConstraintSyntaxValid line currCharIdx (currSeqIdx - 1) elements elemLowerAndUpperBounds --currently not needed as I believe using False in the else statement of isCurrCharBetweenTheseCharsIfSoCheckRemainingChars and isCurrCharBetweenTheseCharsAndAnIntIfSoCheckRemainingCharsAfterInt is correct. This is because I originally did this to account for ints so if you ran into a ')' or ',' and you weren't finished reading the int, it would go back in sequence (so that it would expect and int) but stay on the same char. This implementation does not work as this would allow two AAs to be inputted in a row when you only want one A. Thus, I used a different implementation.
       nextCharIdxAfterInt = currCharIdx + (length $ show intInSequence)
       intInSequence = getIntFromStr (drop currCharIdx line) 0

get2DArrayFromStrs :: [String] -> Int -> Int -> Char -> (String -> Int -> Int -> Int -> Char -> [a] -> [a]) -> [[a]] -> [[a]]
get2DArrayFromStrs fileLines currLineNum numOfCols demarcationChar getIntListFromStrFunc currList
  | currLineNum == (length fileLines) - 1     = currList --when end of file is reached
  | currLine `elem` constraintNames           = currList
  | currLine == "" || (all (== ' ') currLine) = checkNextLine currList
  | otherwise                                 = checkNextLine (currList ++ [getIntListFromStrFunc currLine 0 0 numOfCols demarcationChar []])
  where currLine = fileLines !! currLineNum
        nextLineNum = currLineNum + 1
        checkNextLine = get2DArrayFromStrs fileLines nextLineNum numOfCols demarcationChar getIntListFromStrFunc

getIntListFromStr :: String -> Int -> Int -> Int -> Char -> [Int] -> [Int]
getIntListFromStr line currCharIdx currSeqIdx numOfCols demarcationChar currIntList
 | currSeqIdx == (numOfCols * 2) - 2 = currIntList ++ [intInSequence]
 | seqExpectingDemarcationChar       = getIntListFromStr line nextCharIdx nextSeqIdx numOfCols demarcationChar currIntList
 | seqExpectingElement               = getIntListFromStr line nextCharIdxAfterInt nextSeqIdx numOfCols demarcationChar (currIntList ++ [intInSequence])
 | otherwise                         = [] --this condition should never happen as elements should not let currCharIdx to get bigger than it should be (first guard is the last recursive call)
 where currChar = (line !! currCharIdx)
       nextCharIdx = currCharIdx + 1
       nextSeqIdx = currSeqIdx + 1
       seqExpectingElement = currSeqIdx `mod` 2 == 0 
       seqExpectingDemarcationChar = currChar == demarcationChar
       nextCharIdxAfterInt = currCharIdx + (length $ show intInSequence)
       intInSequence = getIntFromStr (drop currCharIdx line) 0

{-
isOfTypeString :: Typeable a => a -> Bool
isOfTypeString val = (typeOf val == typeOf "String")

isOfTypeInt :: Typeable a => a -> Bool
isOfTypeInt val = ((typeOf 1) == (typeOf val))
-}