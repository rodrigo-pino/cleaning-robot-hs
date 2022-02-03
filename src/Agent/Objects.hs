{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Agent.Objects where

import Control.DeepSeq (NFData)
import Data.Maybe (fromJust, isNothing)
import GHC.Generics (Generic)
import World.Objects

data AgentType = Bold | Cautious

data AssignedTask = AssignedTask {destinaton :: Object, actions :: [Action Position]}
  deriving (Show, Generic, NFData)

data Agent = Agent {entity :: Object, task :: Maybe AssignedTask} deriving (Generic, NFData)

data Solver = Solver {agent :: Agent, time :: Natural} deriving (Show, Generic, NFData)

data Task = Task {target :: Object, solvers :: [Solver]} deriving (Show)

data Natural = Natural Int | Infinite deriving (Show, Generic, NFData)

instance Show Agent where
  show (Agent entity Nothing) = agentEntityShow entity ++ "-Unassigned"
  show (Agent entity (Just aTask)) =
    agentEntityShow entity ++ "-" ++ show (destinaton aTask) ++ show (actions aTask)

agentEntityShow (Object (Robot Nothing) pos) = "Robot:" ++ show pos
agentEntityShow (Object (Robot (Just Kid)) pos) = "Robot(Kid):" ++ show pos

instance Eq Agent where
  a1 == a2 = entity a1 == entity a2

(%==) :: [Agent] -> [Agent] -> Bool
(%==) [] [] = True
(%==) (a : as) (b : bs) = a == b && getTask a == getTask b && as %== bs
(%==) _ _ = False

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
