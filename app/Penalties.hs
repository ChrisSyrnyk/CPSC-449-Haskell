module Penalties
  ( Penalties(..)

  ) where

data Penalties
  = InvalidMachTaskException
  | ParseFileException
  | PartialAssigException
  | InvalidTaskException
  | InvalidPenaltyException
  | MachPenaltyException
  | Penalties { forcedPartialAssigs :: [(Int, Char)]
              , forbiddenMachines :: [(Int, Char)]
              , tooNearTasks :: [(Char, Char)]
              , machinePenalties :: [[Int]]
              , tooNearPenalties :: [(Char, Char, Int)]
              } deriving (Show, Eq)

