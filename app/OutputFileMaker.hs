module OutputFileMaker where

import System.IO

outputFileMakerFunction :: [Char] -> Int -> [Char] -> IO()
outputFileMakerFunction [] penaltyValue fileName = do
    writeFile fileName "No valid solution possible!"
outputFileMakerFunction characterArray penaltyValue fileName = do
    writeFile fileName "Solution"
    writeCharacterArray characterArray fileName
    appendFile fileName ("; Quality: " ++ (show penaltyValue))

writeCharacterArray :: [Char] -> String -> IO()
writeCharacterArray [] fileName = do
    appendFile fileName ""
writeCharacterArray [x] fileName = do
    appendFile fileName " "
    appendFile fileName [x]
writeCharacterArray (x:xs) fileName = do
    appendFile fileName " "
    appendFile fileName [x]
    writeCharacterArray xs fileName 
    
outputWriteErrorMsg :: [Char] -> String -> IO()
outputWriteErrorMsg [] fileName = do
  writeFile fileName ""
outputWriteErrorMsg errorMsg fileName = do
  writeFile fileName ""
  writeErrorMsg errorMsg fileName
    
    
writeErrorMsg :: [Char] -> String -> IO()
writeErrorMsg [] fileName = do
  appendFile fileName ""
writeErrorMsg [x] fileName= do
  appendFile fileName [x]
writeErrorMsg (x:xs) fileName = do
  appendFile fileName [x]
  writeErrorMsg xs fileName

