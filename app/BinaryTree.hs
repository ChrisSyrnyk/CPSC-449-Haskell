module BinaryTree where

import System.Exit

import MyNode
import Penalties
import OutputFileMaker

tasks :: [Char]
tasks = ['A','B','C','D','E','F','G','H']

initialLowestPenaltyValSoFar :: Int
initialLowestPenaltyValSoFar = 9999

initialLowestPenaltyValFromBefore :: (Int, Node)
initialLowestPenaltyValFromBefore = (initialLowestPenaltyValSoFar, Null)

--Data structure for binary tree

data BinaryTree
  = Empty
  | BinaryTree { rootNode :: Node
               , lowestPenaltyValSoFar :: Int
               , nodeWithLowestPenaltyValSoFar :: Node
               , penalties :: Penalties
               } deriving (Show)     --make tree show-able

data Node
  = Null
  | Node { combination :: String
         , parent :: Node
         , childrenArr :: [(Node, Int, Node)]
         , penaltyVal :: Int
         , validOrNot :: Bool
         } deriving (Show)
         

createTreeWithRootNode :: Penalties -> (Node, Int, Node)
createTreeWithRootNode thisTreesPenalties =
  let thisNode = Node [] Null nodesChildrenArr initialLowestPenaltyValSoFar False
      newTree = BinaryTree thisNode initialLowestPenaltyValSoFar Null thisTreesPenalties 
      nodesChildrenArr = getAndCreateChildren (thisNode, initialLowestPenaltyValSoFar, Null) newTree 0 [] 
      thisNodesLowestPenaltyValSoFar = getLowestPenaltyValOfChildren nodesChildrenArr 0 initialLowestPenaltyValFromBefore
  in  (thisNode, fst thisNodesLowestPenaltyValSoFar, snd thisNodesLowestPenaltyValSoFar) 

fromParentAddNode :: (Node, Int, Node) -> BinaryTree -> String -> (Node, Int, Node)
fromParentAddNode nodesParent treeNodeBelongsTo thisNodesCombination
  | thisNodesPenaltyValIsNotLessThanLowestPenaltyValSoFar                                      = thisNodeWithNoChildrenDontUpdateTree
  | not thisNodeIsValid                                                                        = thisNodeWithNoChildrenDontUpdateTree
  | thisNodeAddsLastCharToCombination && thisNodesPenaltyValIsLessThanLowestPenaltyValSoFar    = thisNodeWithNoChildrenUpdateTreeWithNewLowestPenaltyVal
  | otherwise                                                                                  = thisNodeWithChildrenToReturn --when thisNodesPenaltyValEqualToLowestPenaltyValSoFar
  where thisNodeWithNoChildrenDontUpdateTree = (thisNodeWithNoChildren, (getTripletSecond nodesParent), (getTripletThird nodesParent)) 
        thisNodeWithNoChildrenUpdateTreeWithNewLowestPenaltyVal = (thisNodeWithNoChildren, thisNodesPenaltyVal, thisNodeWithNoChildren) --toDo can't figure out how to "update" treeNodeBelongsTo's lowestPenaltyValueSoFar within the confines of Haskell's recursion. For now, both cases thisNodeWithNoChildrenDontUpdateTree and thisNodeWithNoChildrenUpdateTreeWithNewLowestPenaltyVal are the same.
        thisNodeWithNoChildren = Node thisNodesCombination (getTripletFirst nodesParent) [] thisNodesPenaltyVal thisNodeIsValid
        thisNodeWithChildrenForChildren = (Node thisNodesCombination (getTripletFirst nodesParent) nodesChildrenArr thisNodesPenaltyVal thisNodeIsValid, getTripletSecond nodesParent, getTripletThird nodesParent) 
        thisNodeWithChildrenToReturn = (Node thisNodesCombination (getTripletFirst nodesParent) nodesChildrenArr thisNodesPenaltyVal thisNodeIsValid, fst thisNodesLowestPenaltyValSoFar, snd thisNodesLowestPenaltyValSoFar) 
        thisNodeAddsLastCharToCombination = length (combination (getTripletFirst nodesParent)) == length tasks - 1
        thisNodesPenaltyValIsLessThanLowestPenaltyValSoFar = thisNodesPenaltyVal < (getTripletSecond nodesParent)
        thisNodesPenaltyValIsNotLessThanLowestPenaltyValSoFar = thisNodesPenaltyVal >= (getTripletSecond nodesParent)
        --thisNodesCombination = getChildsCombinationFromParents (combination parent)
        thisNodesPenaltyVal = calcNodeTotalPaneltyVal thisNodesCombination machinePenaltiesArr tooNearPenaltiesArr
        thisNodeIsValid = isValid 0 thisNodesCombination (forcedPartialAssigs thisTreesPenalties) (forbiddenMachines thisTreesPenalties) (tooNearTasks thisTreesPenalties) --toDo add Abhay's method here
        nodesChildrenArr = getAndCreateChildren thisNodeWithChildrenForChildren treeNodeBelongsTo 0 [] 
        thisTreesPenalties = penalties treeNodeBelongsTo
        machinePenaltiesArr = machinePenalties (penalties treeNodeBelongsTo)
        tooNearPenaltiesArr = tooNearPenalties (penalties treeNodeBelongsTo)
        thisNodesLowestPenaltyValSoFar = getLowestPenaltyValOfChildren nodesChildrenArr 0 (getTripletSecond nodesParent, getTripletThird nodesParent)

getAndCreateChildren :: (Node, Int, Node) -> BinaryTree -> Int -> [(Node, Int, Node)] -> [(Node, Int, Node)]
getAndCreateChildren nodesParent treeNodesBelongTo currChildToAddIdx currListOfChildren
  | atFinalElementOfChildrenCombinations = listWithCurrChildAdded
  | otherwise                            = addCurrChildThenNextChildren
  where nextChildToAddIdx = currChildToAddIdx + 1
        childrenCombinations = getChildsCombinationFromParents (combination (getTripletFirst nodesParent))
        atFinalElementOfChildrenCombinations = currChildToAddIdx == length childrenCombinations - 1
        listWithCurrChildAdded = currListOfChildren ++ [fromParentAddNode nodesParent treeNodesBelongTo (childrenCombinations !! currChildToAddIdx)]
        addCurrChildThenNextChildren = getAndCreateChildren (getTripletFirst nodesParent, fst theseSiblingsLowestPenaltyValSoFar, snd theseSiblingsLowestPenaltyValSoFar) treeNodesBelongTo nextChildToAddIdx listWithCurrChildAdded
        theseSiblingsLowestPenaltyValSoFar = getLowestPenaltyValOfChildren currListOfChildren 0 initialLowestPenaltyValFromBefore

getLowestPenaltyValOfChildren :: [(Node, Int, Node)] -> Int -> (Int, Node) -> (Int, Node)
getLowestPenaltyValOfChildren listOfChildNodes currIdx lowestPenaltyValFromBefore
  | length listOfChildNodes == 0 = lowestPenaltyValFromBefore
  | currIdx == length listOfChildNodes - 1 = newLowestPenaltyVal
  | otherwise = getLowestPenaltyValOfChildren listOfChildNodes (currIdx + 1) newLowestPenaltyVal
  where currChild = listOfChildNodes !! currIdx
        newLowestPenaltyVal = if currChildLowestPenaltyVal < (fst lowestPenaltyValFromBefore) then (currChildLowestPenaltyVal, currChildNodeWithLowestPenaltyVal) else lowestPenaltyValFromBefore
        currChildLowestPenaltyVal = getTripletSecond currChild
        currChildNodeWithLowestPenaltyVal = getTripletThird currChild

getChildsCombinationFromParents :: String -> [String]
getChildsCombinationFromParents parentsCombination =
  let newList = removeElements 0 0 tasks parentsCombination in childNodes 0 0 newList parentsCombination

--Functions for returing child nodes in tree based off parent node
removeElements :: Int -> Int -> [Char] -> [Char] -> [Char]
--set i and j to 0
--m is the set of all tasks [A->H], n is the node char list
removeElements i j m n =
  if length m == length n then []
  else if i == length m then []                                            --return list
  else if j == length n then [(m!!i)] ++ removeElements (i+1) 0 m n   --return element plus result of function on next i
  else if(m!!i == n!!j) then removeElements (i+1) 0 m n               --check next i and start at first j
  else if (m!!i /= n!!j) then removeElements i (j+1) m n              --check same i with next j
  else ['0']                                                          --Should never reach this statement. If the list produced contains a zero something went wrong

childNodes :: Int -> Int -> [Char] -> [Char] -> [[Char]]
-- set i and j to 0
-- i is used to increment through m and j is used to place the children node in the output list of lists
--m is the char list containing all char's not in parent node
--n is the char list containing from the parent node
childNodes i j m n =
  if length m == 0 then []
  else if i >= length m - 1 then [n ++ [(m!!i)]]
  else [n ++ [(m!!i)]] ++ childNodes (i+1) j m n

--tasks = ['A','B','C','D','E','F','G','H']
--testNode = ['D','G','C']

--getNodeWithLowestPenaltyVal :: [Node] -> Node


{-
addElement :: Int -> Int -> [Char] -> [Char] -> [Char]
--x = tasks; xs == current node array
--i = x incrementer(always set to 0); j = xs incrementer(always set to 0)
addElement i j x xs = 
  if length xs >= 8 then xs
  else if (x!!i == xs!!j && i<length x -1) then addElement (i+1) j x xs
  else if (x!!i /= xs!!j && j<length xs -1) then addElement i (j+1) x xs
  else if (x!!i /= xs!!j && j==(length xs -1)) then xs ++ [x!!i]
  else xs
-}

fromPenaltiesPrintMessage :: Penalties -> String -> IO ()
fromPenaltiesPrintMessage InvalidMachTaskException fileName = do
  --putStr "invalid machine/task"
  outputWriteErrorMsg "invalid machine/task" fileName
  exitSuccess
fromPenaltiesPrintMessage ParseFileException fileName = do
  --putStr "Error while parsing input file"
  outputWriteErrorMsg "Error while parsing input file" fileName
  exitSuccess
fromPenaltiesPrintMessage MachPenaltyException  fileName= do
  --putStr "machine penalty error"
  outputWriteErrorMsg "machine penalty error" fileName
  exitSuccess
fromPenaltiesPrintMessage InvalidPenaltyException fileName = do
  --putStr "invalid penalty"
  outputWriteErrorMsg "invalid penalty" fileName
  exitSuccess
fromPenaltiesPrintMessage pens@(Penalties _ _ _ _ _) fileName = do
  return ()
  
caughtPartialAssigError = 1
noPartialAssigError = 0
ifExceptionFoundInMainPrintMessage :: [(Int,Char)] -> String -> IO ()
ifExceptionFoundInMainPrintMessage partialAssigs fileName
  | length partialAssigs == 0 = return ()
  | otherwise = do
    let isPartialAssigError = partialAssignmentError 0 1 partialAssigs
    errorCheck isPartialAssigError fileName

partialAssignmentError :: Int -> Int -> [(Int,Char)] -> Int
--Always set i to 0, Always set j to 1
--xs is the input [(Int,Char),.....]
partialAssignmentError i j xs
  | i == length xs -1 = noPartialAssigError
  | j == length xs = partialAssignmentError (i+1) (i+2) xs
  | fst(xs!!i) == fst(xs!!j) || snd(xs!!i) == snd(xs!!j) = caughtPartialAssigError
  | otherwise = partialAssignmentError i (j+1) xs

errorCheck :: Int -> String -> IO()
--x is the error value 
errorCheck x fileName
  | x == 1 = do 
    --putStr "partial assignment error"
    outputWriteErrorMsg "partial assignment error" fileName
    exitSuccess
  | x == 2 = do
     --putStr "Place holder for different error"
     outputWriteErrorMsg "Place holder for different error" fileName
     exitSuccess
  | otherwise = return()

getAllNodesInTreeViaRootNode :: [Node] -> [Node]
getAllNodesInTreeViaRootNode currListOfNodes
  | length currListOfNodes == 0 = currListOfNodes
  | otherwise = currListOfNodes ++ getAllNodesInTreeViaRootNode (map getTripletFirst (foldl1 (++) currListOfNodesChildren))
  where currListOfNodesChildren = (map childrenArr currListOfNodes)
  --in

lowestPenalty :: Int->Int->[([Char],Int)] -> ([Char], Int)
--set i to zero. I will hold index current lowest penalty
--set j to one. j will hold index of next penalty value to compare 
lowestPenalty i j xs
  | i == length xs -1 || j == length xs = xs!!i
  | snd(xs!!i) > snd(xs!!j) = lowestPenalty j (j+1) xs
  | otherwise = lowestPenalty i (j+1) xs
--testNodes = [(['A','B'],5),(['B','C'],2),(['C'],0),(['D','A'],10)]


{-
a = [(A,B), (C, D)] !! 0
b = [(A,B), (C, D)] !! 1

tripletA = getTripletFirst a = A
tripletC = getTripletFirst b = C
--compare tripletA with tripletC if both are true, then create new list that gets rid of (A,B) the first occurence

tripletB = getTripletSecond a = B
tripletD = getTripletFirst b = D
-}

{-
  | length currListOfNodes == 0 = []
  | otherwise                   = currListOfNodesChildrenConsolidated
  where currListOfNodesChildren =
  currListOfNodesChildrenConsolidated
currListOfNodesChildren
-}
{-
  --implement tree map
treeMap :: (a -> b) -> BinaryTree a -> BinaryTree b
treeMap _ Empty = Empty
treeMap f (Leaf a) = Leaf (f a)
treeMap f (Node leftSubTree a rightSubTree) = Node (treeMap f leftSubTree) (f a) (treeMap f rightSubTree)
--make mappable
instance Functor BinaryTree where
  fmap = treeMap

  --bt = Node (Node (Leaf 4) 3 Empty) 1 (Leaf 2)
-}
{-
  --implement values preorder traversal
  valuesPreOrder :: BinaryTree a -> [a]
  valuesPreOrder Empty = []
  valuesPreOrder (Leaf a) = [a]
  --first handle value, then left-sub-tree then right-sub-tree
  valuesPreOrder (Node leftSubTree a rightSubTree) = [a] ++ valuesPreOrder leftSubTree ++ valuesPreOrder rightSubTree

  --implement values inOrder traversal
  valuesInOrder :: BinaryTree a -> [a]
  valuesInOrder Empty = []
  valuesInOrder (Leaf a) = [a]
  --first handle left subtree then value then right subtree
  valuesInOrder (Node leftSubTree a rightSubTree) = valuesInOrder leftSubTree ++ [a] ++ valuesInOrder rightSubTree

  --implement values postOrder traversal
  valuesPostOrder :: BinaryTree a -> [a]
  valuesPostOrder Empty = []
  valuesPostOrder (Leaf a) = [a]
  --first handle left subtree then handle right subtree then handle value
  valuesPostOrder (Node leftSubTree a rightSubTree) = valuesPostOrder leftSubTree ++ valuesPostOrder rightSubTree ++ [a]
-}
