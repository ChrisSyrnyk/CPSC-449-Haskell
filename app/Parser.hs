module Parser
  ( callFromMainReturnConstraints
  ) where

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
constraintNames = ["Name:","forced partial assignment:","forbidden machine:","too-near tasks:","machine penalties:","too-near penalities"]
nameStr :: String
nameStr = constraintNames !! 0
forcedPartialAssigsStr :: String
forcedPartialAssigsStr = constraintNames !! 1
forbiddenMachinesStr :: String
forbiddenMachinesStr = constraintNames !! 2
tooNearTasksStr :: String
tooNearTasksStr = constraintNames !! 3
machinePenaltiesStr :: String
machinePenaltiesStr = constraintNames !! 4
tooNearPenaltiesStr :: String
tooNearPenaltiesStr = constraintNames !! 5

constraintInputInvalid :: Int
constraintInputInvalid = -1

forcedPartialAssigNumOfElements :: Int
forcedPartialAssigNumOfElements = (length forcedPartialAssigUpperAndLowerBounds) `div` 2
forcedPartialAssigUpperAndLowerBounds :: [Char]
forcedPartialAssigUpperAndLowerBounds = ['0','9','!','~']

forbiddenMachinesNumOfElements :: Int
forbiddenMachinesNumOfElements = (length forbiddenMachinesUpperAndLowerBounds) `div` 2
forbiddenMachinesUpperAndLowerBounds :: [Char]
forbiddenMachinesUpperAndLowerBounds = ['0','9','!','~']

tooNearTasksNumOfElements :: Int
tooNearTasksNumOfElements = (length tooNearTasksUpperAndLowerBounds) `div` 2
tooNearTasksUpperAndLowerBounds :: [Char]
tooNearTasksUpperAndLowerBounds = ['!','~','!','~']

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
tooNearPenaltiesUpperAndLowerBounds = ['!','~','!','~','0','9']

{-
data ConstraintsFound = ConstraintsFound { forcedPartialAssigsFound :: Bool
                                         , forbiddenMachinesFound :: Bool
                                         , tooNearTasksFound :: Bool
                                         , machinePenaltiesFound :: Bool
                                         , tooNearPenaltiesFound :: Bool
                                         } deriving (Show)
-}

machinePenaltiesInputCorrect :: Int
machinePenaltiesInputCorrect = 0
machinePenaltiesInputMiscIncorrect :: Int
machinePenaltiesInputMiscIncorrect = -1
machinePenaltiesInputMissingNumbers :: Int
machinePenaltiesInputMissingNumbers = -2
machinePenaltiesInputFloatInputted :: Int
machinePenaltiesInputFloatInputted = -3

machinePenaltiesInputParseFileException :: Int
machinePenaltiesInputParseFileException = -1
machinePenaltiesInputMachPenaltyException :: Int
machinePenaltiesInputMachPenaltyException = -2
machinePenaltiesInputInvalidPenaltyException :: Int
machinePenaltiesInputInvalidPenaltyException = -3

tooNearPenaltiesInputCorrect :: Int
tooNearPenaltiesInputCorrect = 0
tooNearPenaltiesInputParseFileException :: Int
tooNearPenaltiesInputParseFileException = -1
tooNearPenaltiesInputInvalidPenaltyException :: Int
tooNearPenaltiesInputInvalidPenaltyException = -2

callFromMainReturnConstraints :: [String] -> Penalties
callFromMainReturnConstraints fileLines = returnConstraints fileLines 0 constraintNames (Penalties [] [] [] [[]] [])

returnConstraints :: [String] -> Int -> [String] -> Penalties -> Penalties
returnConstraints fileLines currLineNum constraintNamesLeft currPenalties
  | lineIsLastLineOfFile && lineIsKeyword = returnConstraints fileLines currLineNum constraintNamesLeftRemoveThisLineKeyword currPenalties
  | lineIsLastLineOfFile                  = if allKeywordsFound then currPenalties else ParseFileException --when end of file is reached
  | currLine == nameStr                   = if lineNumWhenNameInputEndsStored /= constraintInputInvalid then returnConstraints fileLines lineNumWhenNameInputEndsStored constraintNamesLeftRemoveName currPenalties else ParseFileException
  | currLine == forcedPartialAssigsStr    = getForcedPartialAssigs fileLines currLineNum constraintNamesLeft currPenalties
  | currLine == forbiddenMachinesStr      = getForbiddenMachines fileLines currLineNum constraintNamesLeft currPenalties
  | currLine == tooNearTasksStr           = getTooNearTasks fileLines currLineNum constraintNamesLeft currPenalties
  | currLine == machinePenaltiesStr       = getMachinePenalties fileLines currLineNum constraintNamesLeft currPenalties
  | currLine == tooNearPenaltiesStr       = getTooNearPenalties fileLines currLineNum constraintNamesLeft currPenalties
  | otherwise                             = returnConstraints fileLines nextLineNum constraintNamesLeft currPenalties
  where currLine = fileLines !! currLineNum
        nextLineNum = currLineNum + 1
        constraintNamesLeftRemoveName = delete nameStr constraintNamesLeft
        lineNumWhenNameInputEndsStored = lineNumWhenNameInputEnds fileLines nextLineNum constraintNamesLeftRemoveName False
        allKeywordsFound = (length constraintNamesLeft) == 0
        lineIsLastLineOfFile = currLineNum == (length fileLines) - 1
        lineIsKeyword = currLine `elem` constraintNamesLeft
        constraintNamesLeftRemoveThisLineKeyword = delete currLine constraintNamesLeft

getForcedPartialAssigs :: [String] -> Int -> [String] -> Penalties -> Penalties
getForcedPartialAssigs fileLines currLineNum constraintNamesLeft currPenalties
  | (lineNumWhenForcedPartialAssigsInputEnds /= constraintInputInvalid) = returnConstraints fileLines lineNumWhenForcedPartialAssigsInputEnds constraintNamesLeftRemoveForcedPartialAssigs (Penalties forcedPartialAssigsList (forbiddenMachines currPenalties) (tooNearTasks currPenalties) (machinePenalties currPenalties) (tooNearPenalties currPenalties))
  | otherwise                                                           = ParseFileException
  where nextLineNum = currLineNum + 1
        lineNumWhenForcedPartialAssigsInputEnds = lineNumWhenConstraintInputEnds fileLines nextLineNum constraintNamesLeftRemoveForcedPartialAssigs forcedPartialAssigNumOfElements forcedPartialAssigUpperAndLowerBounds
        constraintsIntCharPairList = getPairListFromStrs fileLines nextLineNum getIntCharPairFromStr defaultIntCharPair []
        forcedPartialAssigsList = constraintsIntCharPairList
        constraintNamesLeftRemoveForcedPartialAssigs = delete forcedPartialAssigsStr constraintNamesLeft

getForbiddenMachines :: [String] -> Int -> [String] -> Penalties -> Penalties
getForbiddenMachines fileLines currLineNum constraintNamesLeft currPenalties
  | lineNumWhenForbiddenMachinesInputEnds /= constraintInputInvalid = returnConstraints fileLines lineNumWhenForbiddenMachinesInputEnds constraintNamesLeftRemoveForbiddenMachines (Penalties (forcedPartialAssigs currPenalties) forbiddenMachinesList (tooNearTasks currPenalties) (machinePenalties currPenalties) (tooNearPenalties currPenalties))
  | otherwise                                                       = ParseFileException
  where nextLineNum = currLineNum + 1
        lineNumWhenForbiddenMachinesInputEnds = lineNumWhenConstraintInputEnds fileLines nextLineNum constraintNamesLeftRemoveForbiddenMachines forbiddenMachinesNumOfElements forbiddenMachinesUpperAndLowerBounds
        constraintsIntCharPairList = getPairListFromStrs fileLines nextLineNum getIntCharPairFromStr defaultIntCharPair []
        forbiddenMachinesList = constraintsIntCharPairList
        constraintNamesLeftRemoveForbiddenMachines = delete forbiddenMachinesStr constraintNamesLeft

getTooNearTasks :: [String] -> Int -> [String] -> Penalties -> Penalties
getTooNearTasks fileLines currLineNum constraintNamesLeft currPenalties
  | lineNumWhenTooNearTasksInputEnds /= constraintInputInvalid = returnConstraints fileLines lineNumWhenTooNearTasksInputEnds constraintNamesLeftRemoveTooNearTasks (Penalties (forcedPartialAssigs currPenalties) (forbiddenMachines currPenalties) tooNearTasksList (machinePenalties currPenalties) (tooNearPenalties currPenalties))
  | otherwise                                                  = ParseFileException
  where nextLineNum = currLineNum + 1
        lineNumWhenTooNearTasksInputEnds = lineNumWhenConstraintInputEnds fileLines nextLineNum constraintNamesLeftRemoveTooNearTasks tooNearTasksNumOfElements tooNearTasksUpperAndLowerBounds
        constraintsCharCharPairList = getPairListFromStrs fileLines nextLineNum getCharCharPairFromStr defaultCharCharPair []
        tooNearTasksList = constraintsCharCharPairList
        constraintNamesLeftRemoveTooNearTasks = delete tooNearTasksStr constraintNamesLeft

getMachinePenalties :: [String] -> Int -> [String] -> Penalties -> Penalties
getMachinePenalties fileLines currLineNum constraintNamesLeft currPenalties
  | lineNumWhenMachinePenaltiesInputEndsStored == machinePenaltiesInputParseFileException   = ParseFileException
  | lineNumWhenMachinePenaltiesInputEndsStored == machinePenaltiesInputMachPenaltyException = MachPenaltyException
  | lineNumWhenMachinePenaltiesInputEndsStored == machinePenaltiesInputInvalidPenaltyException = InvalidPenaltyException
  | otherwise                                                                               = returnConstraints fileLines lineNumWhenMachinePenaltiesInputEndsStored constraintNamesLeftRemoveMachinePenalties (Penalties (forcedPartialAssigs currPenalties) (forbiddenMachines currPenalties) (tooNearTasks currPenalties) machinePenaltiesList (tooNearPenalties currPenalties))
  where nextLineNum = currLineNum + 1
        lineNumWhenMachinePenaltiesInputEndsStored = lineNumWhenMachinePenaltiesInputEnds fileLines nextLineNum 0 constraintNamesLeftRemoveMachinePenalties machinePenaltiesRowsAndCols machinePenaltiesUpperAndLowerBounds ' '
        machinePenaltiesList = get2DArrayFromStrs fileLines nextLineNum machinePenaltiesRowsAndCols ' ' getIntListFromStr []
        constraintNamesLeftRemoveMachinePenalties = delete machinePenaltiesStr constraintNamesLeft

getTooNearPenalties :: [String] -> Int -> [String] -> Penalties -> Penalties
getTooNearPenalties fileLines currLineNum constraintNamesLeft currPenalties
  | lineNumWhenTooNearPenaltiesInputEndsStored == tooNearPenaltiesInputParseFileException      = ParseFileException
  | lineNumWhenTooNearPenaltiesInputEndsStored == tooNearPenaltiesInputInvalidPenaltyException = InvalidPenaltyException
  | otherwise                                                                                  = returnConstraints fileLines lineNumWhenTooNearPenaltiesInputEndsStored constraintNamesLeftRemoveTooNearPenalties (Penalties (forcedPartialAssigs currPenalties) (forbiddenMachines currPenalties) (tooNearTasks currPenalties) (machinePenalties currPenalties) tooNearPenaltiesList)
  where nextLineNum = currLineNum + 1
        lineNumWhenTooNearPenaltiesInputEndsStored = lineNumWhenTooNearPenaltiesInputEnds fileLines nextLineNum constraintNamesLeftRemoveTooNearPenalties tooNearPenaltiesNumOfElements tooNearPenaltiesUpperAndLowerBounds
        constraintsCharCharIntTripletList = getTripletListFromStrs fileLines nextLineNum getTripletFromStr defaultCharCharIntTriplet []
        tooNearPenaltiesList = constraintsCharCharIntTripletList
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

lineNumWhenTooNearPenaltiesInputEnds :: [String] -> Int -> [String] -> Int -> [Char] -> Int
lineNumWhenTooNearPenaltiesInputEnds fileLines currLineNum constraintNamesLeft constraintNumOfElements constraintUpperAndLowerBounds
  | not isLastLineOfFile && isBlankLine                                                        = checkNextLine
  | isLastLineOfFile && isBlankLine                                                            = currLineNum
  | currLine `elem` constraintNamesLeft                                                        = currLineNum
  | isTooNearPenaltiesStrSyntaxValidStored == tooNearPenaltiesInputParseFileException          = tooNearPenaltiesInputParseFileException
  | isTooNearPenaltiesStrSyntaxValidStored == tooNearPenaltiesInputInvalidPenaltyException     = tooNearPenaltiesInputInvalidPenaltyException
  | isTooNearPenaltiesStrSyntaxValidStored == tooNearPenaltiesInputCorrect && isLastLineOfFile = currLineNum --when end of file is reached
  | isTooNearPenaltiesStrSyntaxValidStored == tooNearPenaltiesInputCorrect                     = checkNextLine
  | otherwise                                                                                  = constraintInputInvalid
  where currLine = fileLines !! currLineNum
        checkNextLine = lineNumWhenTooNearPenaltiesInputEnds fileLines (currLineNum + 1) constraintNamesLeft constraintNumOfElements constraintUpperAndLowerBounds
        isTooNearPenaltiesStrSyntaxValidStored = isTooNearPenaltiesStrSyntaxValid currLine 0 0 constraintNumOfElements constraintUpperAndLowerBounds
        isLastLineOfFile = currLineNum == (length fileLines) - 1
        isBlankLine = currLine == "" || (all (== ' ') currLine)     
        {-
        | currLine == "" || (all (== ' ') currLine)                                                        = checkNextLine
          | isTooNearPenaltiesStrSyntaxValidStored == tooNearPenaltiesInputParseFileException                = tooNearPenaltiesInputParseFileException
          | isTooNearPenaltiesStrSyntaxValidStored == tooNearPenaltiesInputInvalidPenaltyException           = tooNearPenaltiesInputInvalidPenaltyException
          | currLineNum == (length fileLines) - 1                                                            = currLineNum --when end of file is reached
          | currLine `elem` constraintNamesLeft                                                              = currLineNum
          | isTooNearPenaltiesStrSyntaxValidStored == tooNearPenaltiesInputCorrect                           = checkNextLine
          | otherwise                                                                                        = constraintInputInvalid
-}
lineNumWhenNameInputEnds :: [String] -> Int -> [String] -> Bool -> Int
lineNumWhenNameInputEnds  fileLines currLineNum constraintNamesLeft nameFound
  | currLineNum == (length fileLines) - 1                                                            = currLineNum --when end of file is reached
  | currLine == "" || (all (== ' ') currLine)                                                        = checkNextLine
  | (currLine `elem` constraintNamesLeft) && nameFound                                               = currLineNum
  | not nameFound                                                                                    = checkNextLineNameFound
  | otherwise                                                                                        = constraintInputInvalid
  where currLine = fileLines !! currLineNum
        nextLineNum = currLineNum + 1
        checkNextLine = lineNumWhenNameInputEnds fileLines nextLineNum constraintNamesLeft nameFound
        checkNextLineNameFound =  lineNumWhenNameInputEnds fileLines nextLineNum constraintNamesLeft True

isStringConstraintSyntaxValid :: [Char] -> Int -> Int -> Int -> [Char] -> Bool
isStringConstraintSyntaxValid line currCharIdx currSeqIdx elements elemLowerAndUpperBounds --Bounds are ASCII Codes
  | currSeqIdx == 0                                           = isCurrCharThisCharIfSoCheckRemainingChars '('
  | currSeqAnElement && currSeqBoundsAnInt && currChar == '-' = isCurrCharNegativeInt
  | currSeqAnElement && currSeqBoundsAnInt                    = isCurrCharBetweenTheseCharsAndAnIntIfSoCheckRemainingCharsAfterInt currSeqLowerBound currSeqUpperBound
  | currSeqAnElement                                          = isCurrCharBetweenTheseCharsIfSoCheckRemainingChars currSeqLowerBound currSeqUpperBound
  | currSeqIdx `elem` (take (elements - 1) [2,4..])           = isCurrCharThisCharIfSoCheckRemainingChars ','
  | currSeqIdx == elements * 2                                = isCurrCharThisChar ')' && currCharIdx == length line - 1
  | otherwise                                                 = False --this condition should never happen as elements should not let currSeqIdx to get bigger than it should be
  where currChar = (line !! currCharIdx)
        nextChar = (line !! (currCharIdx + 1))
        currSeqAnElement = currSeqIdx `elem` (take elements [1,3..])
        currSeqBoundsAnInt = currSeqLowerBoundAnInt && currSeqUpperBoundAnInt
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
        isCurrCharNegativeInt = if (nextChar >= '0' && nextChar <= '9') then True && remainingCharsAfterNegativeIntValid else False
        remainingCharsValid = isStringConstraintSyntaxValid line (currCharIdx + 1) (currSeqIdx + 1) elements elemLowerAndUpperBounds
        remainingCharsAfterIntValid = isStringConstraintSyntaxValid line nextCharIdxAfterInt (currSeqIdx + 1) elements elemLowerAndUpperBounds
        --thisCharAndRemainingCharsValid = isStringConstraintSyntaxValid line currCharIdx (currSeqIdx - 1) elements elemLowerAndUpperBounds --currently not needed as I believe using False in the else statement of isCurrCharBetweenTheseCharsIfSoCheckRemainingChars and isCurrCharBetweenTheseCharsAndAnIntIfSoCheckRemainingCharsAfterInt is correct. This is because I originally did this to account for ints so if you ran into a ')' or ',' and you weren't finished reading the int, it would go back in sequence (so that it would expect and int) but stay on the same char. This implementation does not work as this would allow two AAs to be inputted in a row when you only want one A. Thus, I used a different implementation.
        nextCharIdxAfterInt = currCharIdx + (length $ show intInPair)
        intInPair = getIntFromStr (drop currCharIdx line) 0
        remainingCharsAfterNegativeIntValid = isStringConstraintSyntaxValid line nextCharIdxAfterNegativeInt (currSeqIdx + 1) elements elemLowerAndUpperBounds
        nextCharIdxAfterNegativeInt = currCharIdx + (length $ show negativeIntInPair)
        negativeIntInPair = getIntFromStr (drop (currCharIdx) line) 0

isTooNearPenaltiesStrSyntaxValid :: [Char] -> Int -> Int -> Int -> [Char] -> Int
isTooNearPenaltiesStrSyntaxValid line currCharIdx currSeqIdx elements elemLowerAndUpperBounds --Bounds are ASCII Codes
  | currSeqIdx == 0                                           = isCurrCharThisCharIfSoCheckRemainingChars '('
  | currSeqAnElement && currSeqBoundsAnInt && currChar == '-' = isCurrCharNegativeInt
  | currSeqAnElement && currSeqBoundsAnInt                    = isCurrCharBetweenTheseCharsAndAnIntIfSoCheckRemainingCharsAfterInt currSeqLowerBound currSeqUpperBound
  | currSeqAnElement                                          = isCurrCharBetweenTheseCharsIfSoCheckRemainingChars currSeqLowerBound currSeqUpperBound
  | currSeqIdx `elem` (take (elements - 1) [2,4..])           = isCurrCharThisCharIfSoCheckRemainingChars ','
  | isLastSeq                                                 = isCurrCharThisChar ')' + isCurrCharLastChar
  | otherwise                                                 = tooNearPenaltiesInputParseFileException --this condition should never happen as elements should not let currSeqIdx to get bigger than it should be
  where currChar = (line !! currCharIdx)
        nextChar = (line !! (currCharIdx + 1))
        isLastSeq = currSeqIdx == elements * 2
        notLastChar = currCharIdx /= length line - 1
        currSeqAnElement = currSeqIdx `elem` (take elements [1,3..])
        currSeqBoundsAnInt = currSeqLowerBoundAnInt && currSeqUpperBoundAnInt
        currSeqLowerBound = elemLowerAndUpperBounds !! (currSeqIdx - 1)
        currSeqUpperBound = elemLowerAndUpperBounds !! currSeqIdx
        currSeqLowerBoundAnInt = currSeqLowerBound >= '0' && currSeqLowerBound <= '9'
        currSeqUpperBoundAnInt = currSeqUpperBound >= '0' && currSeqUpperBound <= '9'
        isCurrCharThisCharIfSoCheckRemainingChars :: Char -> Int
        isCurrCharThisCharIfSoCheckRemainingChars charShouldBeThisChar = if currChar == charShouldBeThisChar then tooNearPenaltiesInputCorrect + remainingCharsValid else tooNearPenaltiesInputParseFileException
        isCurrCharThisChar :: Char -> Int
        isCurrCharLastChar = if currCharIdx == length line - 1 then tooNearPenaltiesInputCorrect else tooNearPenaltiesInputParseFileException
        isCurrCharThisChar charShouldBeThisChar = if currChar == charShouldBeThisChar then tooNearPenaltiesInputCorrect else tooNearPenaltiesInputParseFileException
        isCurrCharBetweenTheseCharsIfSoCheckRemainingChars :: Char -> Char -> Int
        isCurrCharBetweenTheseCharsIfSoCheckRemainingChars lowerBound upperBound = if currChar >= lowerBound && currChar <= upperBound then tooNearPenaltiesInputCorrect + remainingCharsValid else tooNearPenaltiesInputParseFileException --False here used to be thisCharAndRemainingCharsValid but I think False is correct here
        isCurrCharBetweenTheseCharsAndAnIntIfSoCheckRemainingCharsAfterInt :: Char -> Char -> Int
        isCurrCharBetweenTheseCharsAndAnIntIfSoCheckRemainingCharsAfterInt lowerBound upperBound = if currChar >= lowerBound && currChar <= upperBound then tooNearPenaltiesInputCorrect + remainingCharsAfterIntValid else tooNearPenaltiesInputInvalidPenaltyException --False here used to be thisCharAndRemainingCharsValid but I think False is correct here
        isCurrCharNegativeInt = if notLastChar && (nextChar >= '0' && nextChar <= '9') then tooNearPenaltiesInputInvalidPenaltyException else tooNearPenaltiesInputParseFileException
        --was supposed to go on if statement of isCurrCharNegativeInt but didn't work: && nextCharAfterNegativeIntIsntLastChar && ((not isLastSeq && nextCharAfterNegativeInt == ',') || (isLastSeq && nextCharAfterNegativeInt == ')'))
        remainingCharsValid = isTooNearPenaltiesStrSyntaxValid line (currCharIdx + 1) (currSeqIdx + 1) elements elemLowerAndUpperBounds
        remainingCharsAfterIntValid = isTooNearPenaltiesStrSyntaxValid line nextCharIdxAfterInt (currSeqIdx + 1) elements elemLowerAndUpperBounds
        --thisCharAndRemainingCharsValid = isTooNearPenaltiesStrSyntaxValid line currCharIdx (currSeqIdx - 1) elements elemLowerAndUpperBounds --currently not needed as I believe using False in the else statement of isCurrCharBetweenTheseCharsIfSoCheckRemainingChars and isCurrCharBetweenTheseCharsAndAnIntIfSoCheckRemainingCharsAfterInt is correct. This is because I originally did this to account for ints so if you ran into a ')' or ',' and you weren't finished reading the int, it would go back in sequence (so that it would expect and int) but stay on the same char. This implementation does not work as this would allow two AAs to be inputted in a row when you only want one A. Thus, I used a different implementation.
        nextCharIdxAfterInt = currCharIdx + (length $ show intInPair)
        nextCharAfterNegativeInt = (line !! nextCharIdxAfterNegativeInt)
        nextCharAfterNegativeIntIsntLastChar = nextCharIdxAfterNegativeInt /= (length line) - 1
        intInPair = getIntFromStr (drop currCharIdx line) 0
        remainingCharsAfterNegativeIntValid = isTooNearPenaltiesStrSyntaxValid line nextCharIdxAfterNegativeInt (currSeqIdx + 1) elements elemLowerAndUpperBounds
        nextCharIdxAfterNegativeInt = currCharIdx + (length $ show negativeIntInPair)
        negativeIntInPair = getIntFromStr (drop (currCharIdx) line) 0



getPairListFromStrs :: [String] -> Int -> (String -> Int -> Int -> (a,b) -> (a,b)) -> (a,b) -> [(a,b)] -> [(a,b)]
getPairListFromStrs fileLines currLineNum getPairFromStrFunc defaultPair currPairs
  | lineIsLastLineOfFile && lineIsBlank       = currPairs --when end of file is reached
  | currLine `elem` constraintNames           = currPairs
  | lineIsLastLineOfFile && (not lineIsBlank) = currPairs ++ [getPairFromStrFunc currLine 0 0 defaultPair]
  | lineIsBlank                               = checkNextLine currPairs
  | otherwise                                 = checkNextLine (currPairs ++ [getPairFromStrFunc currLine 0 0 defaultPair])
  where currLine = fileLines !! currLineNum
        nextLineNum = currLineNum + 1
        checkNextLine = getPairListFromStrs fileLines nextLineNum getPairFromStrFunc defaultPair
        lineIsBlank = currLine == "" || (all (== ' ') currLine)
        lineIsLastLineOfFile = currLineNum == (length fileLines) - 1

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
 | lineIsLastLineOfFile && lineIsBlank       = currTriplets --when end of file is reached
 | currLine `elem` constraintNames           = currTriplets
 | lineIsLastLineOfFile && (not lineIsBlank) = currTriplets ++ [getTripletFromStrFunc currLine 0 0 defaultTriplet]
 | lineIsBlank                               = checkNextLine currTriplets
 | otherwise                                 = checkNextLine (currTriplets ++ [getTripletFromStrFunc currLine 0 0 defaultTriplet])
 where currLine = fileLines !! currLineNum
       nextLineNum = currLineNum + 1
       checkNextLine = getTripletListFromStrs fileLines nextLineNum getTripletFromStrFunc defaultTriplet
       lineIsBlank = currLine == "" || (all (== ' ') currLine)
       lineIsLastLineOfFile = currLineNum == (length fileLines) - 1

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
  | currChar == '-' && currCharIdx == 0   = getIntFromStr line nextCharIdx
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
lineNumWhenMachinePenaltiesInputEnds fileLines currLineNum numOfRowsRead constraintNamesLeft cols constraintUpperAndLowerBounds demarcationChar
  | lineIsBlank && lineIsLastLineOfFile && allRowsHaveBeenRead = currLineNum
  | lineIsBlank && lineIsLastLineOfFile && notAllRowsHaveBeenReadYet = machinePenaltiesInputMachPenaltyException
  | lineIsKeyword && allRowsHaveBeenRead                      = currLineNum
  | lineIsKeyword && notAllRowsHaveBeenReadYet                = machinePenaltiesInputMachPenaltyException --not enough valid lines
  | lineIsBlank                                               = lineWhitespaceCheckNextLine
  | lineIsLastLineOfFile && lineIsValid && afterThisLineAllRowsHaveBeenRead  = currLineNum --when end of file is reached
  | lineIsLastLineOfFile && lineIsValid && (not afterThisLineAllRowsHaveBeenRead) = machinePenaltiesInputMachPenaltyException --not enough valid lines
  | lineIsValid && notAllRowsHaveBeenReadYet                  = lineValidCheckNextLine
  | lineIsValid && allRowsHaveBeenRead                        = machinePenaltiesInputMachPenaltyException --too many valid lines
  | lineValidityStatus == machinePenaltiesInputFloatInputted  = machinePenaltiesInputInvalidPenaltyException
  | lineValidityStatus == machinePenaltiesInputMissingNumbers = machinePenaltiesInputMachPenaltyException
  | lineValidityStatus == machinePenaltiesInputMiscIncorrect  = machinePenaltiesInputParseFileException
  | otherwise                                                 = machinePenaltiesInputParseFileException   --should not reach this statement as the above cases should cover all cases
  where currLine = fileLines !! currLineNum
        nextNumOfRowsRead = numOfRowsRead + 1
        lineWhitespaceCheckNextLine = lineNumWhenMachinePenaltiesInputEnds fileLines (currLineNum + 1) numOfRowsRead constraintNamesLeft cols constraintUpperAndLowerBounds demarcationChar
        lineValidCheckNextLine = lineNumWhenMachinePenaltiesInputEnds fileLines (currLineNum + 1) nextNumOfRowsRead constraintNamesLeft cols constraintUpperAndLowerBounds demarcationChar
        lineValidityStatus = isStringMachinePenaltiesSyntaxValid currLine 0 0 cols constraintUpperAndLowerBounds demarcationChar
        lineIsValid = lineValidityStatus == machinePenaltiesInputCorrect
        lineIsBlank = currLine == "" || (all (== ' ') currLine)
        lineIsLastLineOfFile = currLineNum == (length fileLines) - 1
        lineIsKeyword = currLine `elem` constraintNamesLeft
        afterThisLineAllRowsHaveBeenRead = numOfRowsRead + 1 == machinePenaltiesRowsAndCols
        notAllRowsHaveBeenReadYet = numOfRowsRead < machinePenaltiesRowsAndCols
        allRowsHaveBeenRead = numOfRowsRead >= machinePenaltiesRowsAndCols
        --machinePenaltiesInputCorrect
        --machinePenaltiesInputMiscIncorrect
        --machinePenaltiesInputMissingNumbers

isStringMachinePenaltiesSyntaxValid :: [Char] -> Int -> Int -> Int -> [Char] -> Char -> Int
isStringMachinePenaltiesSyntaxValid line currCharIdx currSeqIdx cols elemLowerAndUpperBounds demarcationChar
 | isLastSequence && isntLastChar && nextChar == '.' && twoCharsAfterIsAnInt                                                                                    = machinePenaltiesInputFloatInputted
 | isLastSequence && currChar == '-' && isntLastChar && nextCharIdxAfterNegativeIntEndOfLine                                                                    = isCurrCharBetweenTheseCharsAndANegativeIntAndTheLastCharsOfLine currCharLowerBound currCharUpperBound
 | isLastSequence && currChar == '-' && isntLastChar && (not nextCharIdxAfterNegativeIntEndOfLine)                                                              = machinePenaltiesInputMiscIncorrect
 | isLastSequence && isLastChar                                                                                                                                 = isCurrCharBetweenTheseCharsAndAnIntAndTheLastCharsOfLine currCharLowerBound currCharUpperBound
 | isLastSequence && isntLastChar && isCurrCharAnInt && nextCharIdxAfterIntEndOfLine                                                                            = isCurrCharBetweenTheseCharsAndAnIntAndTheLastCharsOfLine currCharLowerBound currCharUpperBound
 | isLastSequence && isntLastChar && isCurrCharAnInt && (not nextCharIdxAfterIntEndOfLine)                                                                      = machinePenaltiesInputMissingNumbers
 | currChar == demarcationChar && isLastChar && isntLastSequence                                                                                                = machinePenaltiesInputMissingNumbers
 | seqExpectingElement && currCharBoundsAnInt && currChar == '-' && isLastChar                                                                                  = machinePenaltiesInputMiscIncorrect
 | seqExpectingElement && currCharBoundsAnInt && currChar == '-' && isntLastChar && isNextCharAnInt && nextCharIdxAfterNegativeIntEndOfLine && isntLastSequence = machinePenaltiesInputMissingNumbers
 | seqExpectingElement && currCharBoundsAnInt && isCurrCharAnInt && nextCharIdxAfterIntEndOfLine && isntLastSequence                                            = machinePenaltiesInputMissingNumbers
 | seqExpectingElement && currCharBoundsAnInt && currChar == '-'                                                                                                = isCurrCharNegativeInt
 | seqExpectingElement && currCharBoundsAnInt                                                                                                                   = isCurrCharBetweenTheseCharsAndAnIntIfSoCheckRemainingCharsAfterInt currCharLowerBound currCharUpperBound
 | seqExpectingDemarcationChar && currChar == '.' && isntLastChar && nextCharIsAnInt                                                                            = machinePenaltiesInputFloatInputted
 | seqExpectingDemarcationChar                                                                                                                                  = isCurrCharThisCharIfSoCheckRemainingChars demarcationChar
 | otherwise                                                                                                                                                    = machinePenaltiesInputMiscIncorrect --this condition should never happen as elements should not let currCharIdx to get bigger than it should be (first guard is the last recursive call)
 where currChar = (line !! currCharIdx)
       nextChar = (line !! (currCharIdx + 1))
       charAfterInt = (line !! nextCharIdxAfterInt)
       nextCharIsAnInt = nextChar >= '0' && nextChar <= '9'
       twoCharsAfterIsAnInt = (line !! (currCharIdx + 2)) >= '0' && (line !! (currCharIdx + 2)) <= '9'
       nextCharIdxAfterCurrCharAndIntIsEndOfLine = nextCharIdxAfterCurrCharAndInt == length line
       nextCharIdxAfterCurrCharAndIntNotEndOfLine = nextCharIdxAfterCurrCharAndInt /= length line
       nextCharIdx = currCharIdx + 1
       nextSeqIdx = currSeqIdx + 1
       isLastChar = currCharIdx == ((length line) - 1)
       isntLastChar = not isLastChar
       isLastSequence = currSeqIdx == (cols * 2) - 2
       isntLastSequence = not isLastSequence
       seqExpectingElement = currSeqIdx `mod` 2 == 0
       seqExpectingDemarcationChar = currSeqIdx `mod` 2 == 1
       currCharBoundsAnInt = (currCharLowerBoundAnInt && currCharUpperBoundAnInt)
       currCharLowerBound = elemLowerAndUpperBounds !! 0
       currCharUpperBound = elemLowerAndUpperBounds !! 1
       currCharLowerBoundAnInt = currCharLowerBound >= '0' && currCharLowerBound <= '9'
       currCharUpperBoundAnInt = currCharUpperBound >= '0' && currCharUpperBound <= '9'
       isCurrCharThisCharIfSoCheckRemainingChars :: Char -> Int
       isCurrCharThisCharIfSoCheckRemainingChars charShouldBeThisChar = if currChar == charShouldBeThisChar then machinePenaltiesInputCorrect + remainingCharsValid else machinePenaltiesInputMiscIncorrect
       --isCurrCharThisChar :: Char -> Bool
       --isCurrCharThisChar charShouldBeThisChar = if currChar == charShouldBeThisChar then True else False
       --isCurrCharBetweenTheseCharsIfSoCheckRemainingChars :: Char -> Char -> Bool
       --isCurrCharBetweenTheseCharsIfSoCheckRemainingChars lowerBound upperBound = if currChar >= lowerBound && currChar <= upperBound then True && remainingCharsValid else False --False here used to be thisCharAndRemainingCharsValid but I think False is correct here
       isCurrCharBetweenTheseCharsAndAnIntIfSoCheckRemainingCharsAfterInt :: Char -> Char -> Int
       isCurrCharBetweenTheseCharsAndAnIntIfSoCheckRemainingCharsAfterInt lowerBound upperBound = if currChar >= lowerBound && currChar <= upperBound then machinePenaltiesInputCorrect + remainingCharsAfterIntValid else machinePenaltiesInputMiscIncorrect --False here used to be thisCharAndRemainingCharsValid but I think False is correct here
       isCurrCharBetweenTheseCharsAndAnIntAndTheLastCharsOfLine :: Char -> Char -> Int
       isCurrCharBetweenTheseCharsAndAnIntAndTheLastCharsOfLine lowerBound upperBound = if (currChar >= lowerBound) && (currChar <= upperBound) && (nextCharIdxAfterInt == (length line)) then machinePenaltiesInputCorrect else machinePenaltiesInputMiscIncorrect
       isCurrCharBetweenTheseCharsAndANegativeIntAndTheLastCharsOfLine lowerBound upperBound = if (nextChar >= lowerBound) && (nextChar <= upperBound) && (nextCharIdxAfterNegativeInt == (length line)) then machinePenaltiesInputCorrect else machinePenaltiesInputMiscIncorrect
       remainingCharsValid = isStringMachinePenaltiesSyntaxValid line nextCharIdx nextSeqIdx cols elemLowerAndUpperBounds demarcationChar
       remainingCharsAfterIntValid = isStringMachinePenaltiesSyntaxValid line nextCharIdxAfterInt nextSeqIdx cols elemLowerAndUpperBounds demarcationChar
       --thisCharAndRemainingCharsValid = isStringConstraintSyntaxValid line currCharIdx (currSeqIdx - 1) elements elemLowerAndUpperBounds --currently not needed as I believe using False in the else statement of isCurrCharBetweenTheseCharsIfSoCheckRemainingChars and isCurrCharBetweenTheseCharsAndAnIntIfSoCheckRemainingCharsAfterInt is correct. This is because I originally did this to account for ints so if you ran into a ')' or ',' and you weren't finished reading the int, it would go back in sequence (so that it would expect and int) but stay on the same char. This implementation does not work as this would allow two AAs to be inputted in a row when you only want one A. Thus, I used a different implementation.
       nextCharIdxAfterInt = currCharIdx + (length $ show intInSequence)
       nextCharIdxAfterCurrCharAndInt = nextCharIdx + (length $ show intInSequenceAfterCurrChar)
       isCurrCharAnInt = (currChar >= '0' && currChar <= '9')
       isNextCharAnInt = (nextChar >= '0' && nextChar <= '9')
       isCurrCharNegativeInt = if isNextCharAnInt then machinePenaltiesInputCorrect + remainingCharsAfterNegativeIntValid else machinePenaltiesInputMiscIncorrect
       intInSequence = getIntFromStr (drop currCharIdx line) 0
       intInSequenceAfterCurrChar = getIntFromStr (drop nextCharIdx line) 0
       remainingCharsAfterNegativeIntValid = isStringMachinePenaltiesSyntaxValid line nextCharIdxAfterNegativeInt nextSeqIdx cols elemLowerAndUpperBounds demarcationChar
       nextCharIdxAfterNegativeInt = currCharIdx + (length $ show negativeIntInPair)
       negativeIntInPair = getIntFromStr (drop (currCharIdx) line) 0
       nextCharIdxAfterIntEndOfLine = nextCharIdxAfterInt == (length line)
       nextCharIdxAfterNegativeIntEndOfLine = nextCharIdxAfterNegativeInt == (length line)


{-
machinePenaltiesInputCorrect
machinePenaltiesInputMiscIncorrect
machinePenaltiesInputMissingNumbers
-}
get2DArrayFromStrs :: [String] -> Int -> Int -> Char -> (String -> Int -> Int -> Int -> Char -> [a] -> [a]) -> [[a]] -> [[a]]
get2DArrayFromStrs fileLines currLineNum numOfCols demarcationChar getIntListFromStrFunc currList
  | lineIsLastLineOfFile && lineIsBlank       = currList --when end of file is reached
  | currLine `elem` constraintNames           = currList
  | lineIsLastLineOfFile && (not lineIsBlank) = currList ++ [intListFromLine]
  | lineIsBlank                               = checkNextLine currList
  | otherwise                                 = checkNextLine (currList ++ [intListFromLine])
  where currLine = fileLines !! currLineNum
        nextLineNum = currLineNum + 1
        checkNextLine = get2DArrayFromStrs fileLines nextLineNum numOfCols demarcationChar getIntListFromStrFunc
        lineIsBlank = currLine == "" || (all (== ' ') currLine)
        lineIsLastLineOfFile = currLineNum == (length fileLines) - 1
        intListFromLine = getIntListFromStrFunc currLine 0 0 numOfCols demarcationChar []

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
