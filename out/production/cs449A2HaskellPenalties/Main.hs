module Main where

import Lib
import System.Environment
import Data.List
--import Control.Monad

import Parser

main :: IO ()
main = do
  args <- getArgs
  let textFileSuffix = ".txt"
      inputFileName = (head args) ++ textFileSuffix
      outputFileName = (args !! 1) ++ textFileSuffix
  parseFileHandleExceptions inputFileName
  putStrLn inputFileName
  putStrLn outputFileName

  return ()
