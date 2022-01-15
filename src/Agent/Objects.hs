module Agent.Objects where

import World.Objects

data Agent = Agent {entity :: Object, task :: Maybe AssignedTask}

data Solver = Solver {agent :: Agent, time :: Natural}

data Task = Task {target :: Object, solvers :: [Solver]}

data AssignedTask = AssignedTask {destinaton :: Object, actions :: [Action Position]}

data Natural = Natural Int | Infinite

instance Eq Agent where
  a1 == a2 = entity a1 == entity a2

instance Eq Task where
  m1 == m2 = target m1 == target m2

instance Eq Natural where
  Infinite == Infinite = True
  Natural a1 == Natural a2 = a1 == a2
  n1 == n2 = False

instance Ord Natural where
  Natural a1 <= Natural a2 = a1 == a2
  Natural _ <= Infinite = True
  Infinite <= Natural _ = False
  Infinite <= Infinite = False

instance Num Natural where
  (+) n1 n2 = calc (+) n1 n2

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
