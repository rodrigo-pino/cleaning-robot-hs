module Agent.Objects where

import Data.Maybe (fromJust, isNothing)
import World.Objects

data AssignedTask = AssignedTask {destinaton :: Object, actions :: [Action Position]}
  deriving (Show)

data Agent = Agent {entity :: Object, task :: Maybe AssignedTask}

data Solver = Solver {agent :: Agent, time :: Natural} deriving (Show)

data Task = Task {target :: Object, solvers :: [Solver]} deriving (Show)

data Natural = Natural Int | Infinite deriving (Show)

instance Show Agent where
  show (Agent entity Nothing) = "Agent: " ++ show entity ++ " Unassigned"
  show (Agent entity (Just aTask)) =
    "Agent: " ++ show entity ++ " Assigned to: " ++ show (destinaton aTask)

instance Eq Agent where
  a1 == a2 = entity a1 == entity a2

instance Eq Solver where
  s1 == s2 = agent s1 == agent s2 && time s1 == time s2

instance Eq Task where
  m1 == m2 = target m1 == target m2 && solvers m1 == solvers m2

instance Eq Natural where
  Infinite == Infinite = True
  Natural a1 == Natural a2 = a1 == a2
  n1 == n2 = False

instance Ord Natural where
  Natural a1 <= Natural a2 = a1 <= a2
  Natural _ <= Infinite = True
  Infinite <= Natural _ = False
  Infinite <= Infinite = False

instance Num Natural where
  (+) n1 n2 = calc (+) n1 n2
  (-) n1 n2 = calc (-) n1 n2
  (*) n1 n2 = calc (*) n1 n2
  fromInteger n = Natural (fromIntegral n)

-- fromInteger n = Natural n

calc op val1 val2
  | val1 == Infinite || val2 == Infinite = Infinite
  | otherwise =
    let Natural a1 = val1
        Natural a2 = val2
     in Natural (op a1 a2)

alreadyAssigned _ [] = False
alreadyAssigned task (aTask : aTasks)
  | target task == destinaton aTask = True
  | otherwise = alreadyAssigned task aTasks

getTask :: Agent -> Maybe Object
getTask ag =
  if isNothing maybeTask
    then Nothing
    else Just (destinaton justTask)
  where
    maybeTask = task ag
    justTask = fromJust maybeTask

unassingAgent (Agent obj _) = Agent obj Nothing
