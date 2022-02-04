module Agent.Logic.Pathfinding.Find where

import Agent.Logic.Pathfinding.Algorithm
import Agent.Logic.Pathfinding.PathCalculation
import Agent.Objects (Agent (..), Natural (..), Solver (..), Task (..), getTask)
import Control.DeepSeq (deepseq)
import Control.Parallel (par, pseq)
import Data.Maybe (fromJust, isJust)
import Debug.Trace (trace)
import GHC.List (foldl')
import World.Objects (Action (..), Board (..), Object (..), ObjectType (..), Position (..))
import qualified World.Objects as WO

findSolversOld :: Board -> [Task] -> [Agent] -> PathCalcType -> [Task]
findSolversOld _ tasks [] _ =
  [ Task {target = targetx, solvers = reverse solversx}
    | taskx <- tasks,
      let targetx = target taskx,
      let solversx = solvers taskx,
      not (null solversx)
  ]
findSolversOld board tasks (ag : agents) calcType = findSolversOld board updatedTasks agents calcType
  where
    foundTasks = reachableTasks (robotlessBoard board ag) ag calcType
    updatedTasks =
      [ newTask
        | taskx <- tasks,
          let solversx = solvers taskx,
          let targetx = target taskx,
          let time = lookup targetx foundTasks,
          let newTask =
                if isJust time
                  then Task targetx (Solver ag (fromJust time) : solversx)
                  else taskx
      ]

findSolvers :: Board -> [Task] -> [Agent] -> PathCalcType -> [Task]
findSolvers board tasks agents calcType = updatedTasks
  where
    -- Task Updating
    updatedTasks = g tasks
    g [] = []
    g (t : ts) = newTask `par` (restTasks `pseq` newTask : restTasks)
      where
        -- recursion
        restTasks = g ts
        -- call logic
        newTask = Task targetx solvers
        targetx = target t
        solvers =
          [ Solver ag (fromJust time)
            | (ag, rt) <- zip agents reachedTasks,
              let time = lookup targetx rt,
              isJust time
          ]

    -- Task Searching
    reachedTasks = f agents
    f [] = []
    f (x : xs) = rt `par` rts `pseq` (rt : rts)
      where
        rt = reachableTasks (robotlessBoard board x) x calcType
        rts = f xs

findObjects :: Board -> [Agent] -> [Object] -> PathCalcType -> [[Action Position]]
findObjects board agents targets calcType = paths
  where
    paths = f agents targets
    f [] [] = []
    f (ag : ags) (tg : tgs) = p `par` (ps `pseq` p : ps)
      where
        -- recursion
        ps = f ags tgs
        -- logic
        p = pathToTask board ag tg calcType

findTargets :: Board -> [Agent] -> PathCalcType -> [[Action Position]]
findTargets board agents = findObjects board agents (map (fromJust . getTask) agents)

findObject :: Board -> Agent -> Object -> PathCalcType -> [Action Position]
findObject board ag targetx calcType =
  let bestPath = pathToTask board ag targetx calcType
   in -- (bestPath, _) = foldl' takeMin ([], Infinite) paths
      reverse bestPath
  where
    takeMin acc@(_, accCost) val@(_, valCost) =
      if valCost < accCost then val else acc

findTarget :: Board -> Agent -> PathCalcType -> [Action Position]
findTarget board ag = findObject board ag ((fromJust . getTask) ag)

robotlessBoard :: Board -> Agent -> Board
robotlessBoard board ag =
  let newElems = filter f (WO.elems board)
      f obj@(Object (Robot _) pos) = obj == entity ag
      f _ = True
   in Board newElems (WO.maxRows board) (WO.maxCols board)
