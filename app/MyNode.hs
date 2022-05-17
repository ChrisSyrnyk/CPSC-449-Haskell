module MyNode where

getTripletFirst :: (a,b,c) -> a
getTripletFirst (x,_,_) = x

getTripletSecond :: (a,b,c) -> b
getTripletSecond (_,y,_) = y

getTripletThird :: (a,b,c) -> c
getTripletThird (_,_,z) = z

-- Everytime you call this funtion remember to put 0 for rewNum variable.
calcMachinePenalties :: Int -> [Char] -> [[Int]] -> Int
calcMachinePenalties rowNum [] machinePanelties = 0
calcMachinePenalties rowNum [x] machinePanelties 
    | x == 'A' = ((machinePanelties !! rowNum) !! 0)
    | x == 'B' = ((machinePanelties !! rowNum) !! 1)
    | x == 'C' = ((machinePanelties !! rowNum) !! 2)
    | x == 'D' = ((machinePanelties !! rowNum) !! 3)
    | x == 'E' = ((machinePanelties !! rowNum) !! 4)
    | x == 'F' = ((machinePanelties !! rowNum) !! 5)
    | x == 'G' = ((machinePanelties !! rowNum) !! 6)
    | x == 'H' = ((machinePanelties !! rowNum) !! 7)
calcMachinePenalties rowNum (x:xs) machinePanelties 
    = (calcMachinePenalties rowNum [x] machinePanelties) + (calcMachinePenalties (rowNum + 1) xs machinePanelties)
   
calcTooNearPaneltyValues :: Int -> [Char] -> [(Char, Char, Int)] -> Int
calcTooNearPaneltyValues colNum combinationArray tooNearPenalties 
    =   if colNum < (length combinationArray) 
        then (tooNearPaneltiesChecker colNum (length combinationArray) (combinationArray !! colNum) (combinationArray !! ((colNum + 1) `rem` (length combinationArray))) tooNearPenalties) + (calcTooNearPaneltyValues (colNum + 1) combinationArray tooNearPenalties)
        else 0  

tooNearPaneltiesChecker :: Int -> Int -> Char -> Char -> [(Char, Char, Int)] -> Int
tooNearPaneltiesChecker colNum combinationArrayLength firstElement secondElement [] = 0
tooNearPaneltiesChecker colNum combinationArrayLength firstElement secondElement [x] 
    | colNum == (combinationArrayLength - 1) && colNum /= 7 = 0
    | firstElement == (getTripletFirst x) && secondElement == (getTripletSecond x) = (getTripletThird x)
    | otherwise = 0
tooNearPaneltiesChecker colNum combinationArrayLength firstElement secondElement (x:xs)
    = (tooNearPaneltiesChecker colNum combinationArrayLength firstElement secondElement [x]) + (tooNearPaneltiesChecker colNum combinationArrayLength firstElement secondElement xs)

calcNodeTotalPaneltyVal :: [Char] -> [[Int]] -> [(Char, Char, Int)] -> Int
calcNodeTotalPaneltyVal combinationArray machinePenelties tooNearPenalties = 
    let a = calcMachinePenalties 0 combinationArray machinePenelties
        b = calcTooNearPaneltyValues 0 combinationArray tooNearPenalties
    in (a + b)


--This function taked in the character array is an input and tells if the node with this character array is valid or not.  
isValid :: Int -> [Char] -> [(Int, Char)] -> [(Int, Char)] -> [(Char, Char)] -> Bool
isValid column characterArray forcedPartialAssigs forbiddenMachines tooNearTasks = 
    if column < (length characterArray)
        then (isPartialAssignmentCorrectChecker column (characterArray !! column) forcedPartialAssigs) && (not (isForbidden column (characterArray !! column) forbiddenMachines)) && (not (checkTooNearTasks 0 characterArray tooNearTasks))&& (isValid (column + 1) characterArray forcedPartialAssigs forbiddenMachines tooNearTasks) 
        else True

-- This function takes in the column number (machine number), character(task) and the forced assignment array. and checks if the machine and task pair is in the forcedpartial assignment array or not.  
isPartialAssignmentCorrectChecker :: Int -> Char -> [(Int, Char)] -> Bool
isPartialAssignmentCorrectChecker column assignedTask [] = True
isPartialAssignmentCorrectChecker column assignedTask [x]
    | (fst x) == (column + 1) && (snd x) == assignedTask = True
    | (fst x) == (column + 1) && (snd x) /= assignedTask = False
    | otherwise = True
isPartialAssignmentCorrectChecker column assignedTask (x:xs)
    = (isPartialAssignmentCorrectChecker column assignedTask [x]) && isPartialAssignmentCorrectChecker column assignedTask xs 


-- This function returns a False if a machine task pair is not forbidden. Returns true otherwise 
isForbidden :: Int -> Char -> [(Int, Char)] -> Bool
isForbidden column assignedTask [] = False 
isForbidden column assignedTask [x] 
    | (fst x) == (column + 1) && (snd x) == assignedTask = True
    | (fst x) == (column + 1) && (snd x) /= assignedTask = False
    | otherwise = False
isForbidden column assignedTask (x:xs)
    = (isForbidden column assignedTask [x]) || (isForbidden column assignedTask xs)

--This function is to find if a node has too near penlties or not
--Returns true if the node has a true near penalty
checkTooNearTasks :: Int -> [Char] -> [(Char, Char)] -> Bool
checkTooNearTasks colNum combinationArray tooNearTasks 
    =   if colNum < (length combinationArray) 
        then (tooNearTasksChecker colNum (length combinationArray) (combinationArray !! colNum) (combinationArray !! ((colNum + 1) `rem` (length combinationArray))) tooNearTasks) || (checkTooNearTasks (colNum + 1) combinationArray tooNearTasks)
        else False  

tooNearTasksChecker :: Int -> Int -> Char -> Char -> [(Char, Char)] -> Bool
tooNearTasksChecker colNum combinationArrayLength firstElement secondElement [] = False
tooNearTasksChecker colNum combinationArrayLength firstElement secondElement [x] 
    | colNum == (combinationArrayLength - 1) && colNum /= 7 = False
    | firstElement == (fst x) && secondElement == (snd x) = True
    | otherwise = False
tooNearTasksChecker colNum combinationArrayLength firstElement secondElement (x:xs)
    = (tooNearTasksChecker colNum combinationArrayLength firstElement secondElement [x]) || (tooNearTasksChecker colNum combinationArrayLength firstElement secondElement xs)


--This function is to remove any duplicates from the too Near task Penalty array. It returns an corrected too near penalty array that does not contains any duplicates. Call the main function to use this...
search :: (Char,Char,Int) -> [(Char,Char,Int)] -> Bool
search a [] = False
search (a,b,num) ((c,d,_):xs) = ((a==c) && (b == d)) || search (a,b,num) xs

mainFunc :: [(Char,Char,Int)] -> [(Char,Char,Int)]
mainFunc [] = []
mainFunc x = reverse (recpp (reverse x) []) 

recpp ::  [(Char,Char,Int)] -> [(Char,Char,Int)] -> [(Char,Char,Int)]
recpp [] b = b
recpp (x:xs) b = if search x b then recpp xs b
                             else recpp xs (b++[x])

-- -------------------------------------------------
