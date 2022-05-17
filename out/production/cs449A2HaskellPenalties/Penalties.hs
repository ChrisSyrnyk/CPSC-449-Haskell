module Penalties
  ( Penalties(..)

  ) where

data Penalties = Penalties { forcedPartialAssigs :: [(Int, Char)]
                           , forbiddenMachines :: [(Int, Char)]
                           , tooNearTasks :: [(Char, Char)]
                           , machinePenalties :: [[Int]]
                           , tooNearPenalties :: [(Char, Char, Int)]
                           } deriving (Show)