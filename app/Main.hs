module Main where

import Lib
import System.Environment
import Data.List
import BinaryTree
import Parser
import Penalties
import OutputFileMaker
import ErrorCheck
import MyNode
import Penalties (Penalties(machinePenalties, tooNearPenalties, Penalties, forbiddenMachines, tooNearTasks))
import Control.Exception

{-
data InvalidTooNearPenaltyException = InvalidTooNearPenaltyException deriving Show

instance Exception InvalidTooNearPenaltyException
-}


--create binary tree for testing
--bt = Node (Node (Leaf 3) 2 (Leaf 5)) 1 (Node (Leaf 6) 3 (Leaf 7)) --create binary tree
{-
                                     1
                                   /   \
                                  2     3
                                 /\     /\
                                4  5   6  7
-}
--ct = fmap (*4) bt --test map function multiply all tree values by 4 and store in new tree

main :: IO ()
main = do
  args <- getArgs
  let inputFileName = (head args)
      outputFileName = (args !! 1)
  fileContents <- readFile inputFileName
  let fileLines = lines fileContents
      rawPenalties = callFromMainReturnConstraints fileLines
      --rawPenalties = callFromMainReturnConstraints fileLines `catch` \ (ex :: SomeException) -> handleException
  {-
  catch callFromMainReturnConstraints fileLines (\e -> do

        putStrLn "invalid penalty"
        )--These are the default "instance variable" values for the penalty "object"
  callFromMainReturnConstraints fileLines
  -}
  let rootNodeTuple = createTreeWithRootNode rawPenalties
      rootNode = MyNode.getTripletFirst rootNodeTuple
      allNodes = getAllNodesInTreeViaRootNode [rootNode]
      allNodesCombinations = map combination allNodes
      allNodesPenalties = map penaltyVal allNodes
      allNodesValidity = map validOrNot allNodes
      allNodesCombinationsPenalties = zip3 allNodesCombinations allNodesPenalties allNodesValidity
      allNodesCombinationsPenaltiesValid = zip allNodesCombinations allNodesPenalties
      nodesWith8Chars = filter (\x -> (length (combination x)) == 8) allNodes
      --or you could do a list comprehension: nodesWith8Chars = [x | x <- allNodes, (length (combination x)) == 8]
      nodeWithLowestPenaltyValCandidates = filter validOrNot nodesWith8Chars
      --or you could do a list comprehension: [x | x <- nodesWith8Chars, isValid x]
      nodeWithLowestPenaltyValCandidatesCombinations = map combination nodeWithLowestPenaltyValCandidates
      nodeWithLowestPenaltyValCandidatesPenalties = map penaltyVal nodeWithLowestPenaltyValCandidates
      nodeWithLowestPenaltyValCandidatesCombinationsPenalties = zip nodeWithLowestPenaltyValCandidatesCombinations nodeWithLowestPenaltyValCandidatesPenalties
      noNodeWithLowestPenaltyValCandidates = length nodeWithLowestPenaltyValCandidates == 0
      --lowestNode = lowestPenalty 0 1 nodeWithLowestPenaltyValCandidatesCombinationsPenalties
      lowestNode = getTripletThird rootNodeTuple
  fromPenaltiesPrintMessage rawPenalties outputFileName
  ifExceptionFoundInMainPrintMessage (forcedPartialAssigs rawPenalties) outputFileName
  -- print allNodesCombinationsPenalties
  -- print (MyNode.getTripletSecond rootNodeTuple)
  -- print (combination (MyNode.getTripletThird rootNodeTuple))

  let tooNearTaskErrorCheckList = invalidTaskChecker (tooNearPenalties rawPenalties)
      invalidTaskErrorTooNearTask = invalidTaskError asciiTaskList 
        where 
          asciiTaskList = charToInt 0 tooNearTaskErrorCheckList
  let rawPenaltiesCheck = invalidPenaltyCheck machinePenaltiesFilterInput
        where 
          machinePenaltiesFilterInput = machinePenaltiesFilter 0 (machinePenalties rawPenalties)
  let invalidForcedPartialAssignErrorChecker = invalidMachineTaskCheck 0 (forcedPartialAssigs rawPenalties)
  let invalidForbiddenMachineErrorChecker = invalidMachineTaskCheck 0 (forbiddenMachines rawPenalties)
  let invalidTooNearTaskChecker = invalidTooNearCheck 0 (tooNearTasks rawPenalties)

  if noNodeWithLowestPenaltyValCandidates then printOutputFile lowestNode outputFileName else do
  --Code for error checking ------------------
  --Checks for "invalid penalty" and "invalid task"
  --Output the correspond error and terminate the program
    errorerrorCheck invalidForcedPartialAssignErrorChecker outputFileName
    errorerrorCheck invalidTooNearTaskChecker outputFileName
    errorerrorCheck invalidForbiddenMachineErrorChecker outputFileName
    errorerrorCheck invalidTaskErrorTooNearTask outputFileName
    errorerrorCheck rawPenaltiesCheck outputFileName
  -- Code for error checking ------------------

    printOutputFile lowestNode outputFileName
  --if noNodeWithLowestPenaltyValCandidates then outputFileMakerFunction [] 10 outputFileName else outputFileMakerFunction (fst lowestNode) (snd lowestNode) outputFileName
{-
  print (combination rootNode)
  print (combination (head (childrenArr rootNode)))
  print (combination (head (childrenArr (head (childrenArr rootNode)))))
  print (combination (head (childrenArr (head (childrenArr (head (childrenArr rootNode)))))))
  print (combination (head (childrenArr (last (childrenArr (last (childrenArr (head (childrenArr rootNode)))))))))
  print (combination (head (childrenArr (head (childrenArr (head (childrenArr (head (childrenArr rootNode)))))))))
  print (combination (head (childrenArr (head (childrenArr (head (childrenArr (head (childrenArr (head (childrenArr rootNode)))))))))))
  print (combination (head (childrenArr (head (childrenArr (head (childrenArr (head (childrenArr (head (childrenArr (head (childrenArr rootNode)))))))))))))
  print (combination (head (childrenArr (head (childrenArr (head (childrenArr (head (childrenArr (head (childrenArr (head (childrenArr (head (childrenArr rootNode)))))))))))))))
  print (combination (head (childrenArr (head (childrenArr (head (childrenArr (head (childrenArr (head (childrenArr (head (childrenArr (head (childrenArr (head (childrenArr rootNode)))))))))))))))))
  print (combination (head (childrenArr (head (childrenArr (last (childrenArr (head (childrenArr (head (childrenArr (head (childrenArr (head (childrenArr (head (childrenArr rootNode)))))))))))))))))
  --print (combination (head (childrenArr (head (childrenArr (head (childrenArr (head (childrenArr (head (childrenArr (head (childrenArr (head (childrenArr (head (childrenArr (head (childrenArr rootNode)))))))))))))))))))
  --print (combination (head (childrenArr (head (childrenArr (head (childrenArr (head (childrenArr (head (childrenArr (head (childrenArr (head (childrenArr (head (childrenArr (head (childrenArr (head (childrenArr rootNode)))))))))))))))))))))
-}
  return ()

printOutputFile :: Node -> String -> IO ()
printOutputFile Null outputFileName = outputFileMakerFunction [] 10 outputFileName
printOutputFile (Node nodesCombination _ _ nodesPenaltyVal _) outputFileName = outputFileMakerFunction nodesCombination nodesPenaltyVal outputFileName



testPrintNodeCombinations :: Node -> IO ()
testPrintNodeCombinations nodeToPrint = do
  print (combination nodeToPrint)
  return ()
