module Agent.Logic.Pathfinding.Find where

import Agent.Logic.Pathfinding.Algorithm
import Agent.Logic.Pathfinding.PathCalculation
import Agent.Objects (Agent (..), Natural (..), Solver (..), Task (..), getTask)
import Data.Maybe (fromJust, isJust)
import Debug.Trace (trace)
import GHC.List (foldl')
import World.Objects (Action (..), Board (..), Object (..), ObjectType (..), Position (..))
import qualified World.Objects as WO

findSolvers :: Board -> [Task] -> [Agent] -> PathCalcType -> [Task]
findSolvers _ tasks [] _ =
  [ Task {target = targetx, solvers = reverse solversx}
    | taskx <- tasks,
      let targetx = target taskx,
      let solversx = solvers taskx,
      not (null solversx)
  ]
findSolvers board tasks (ag : agents) calcType = findSolvers board updatedTasks agents calcType
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

findObject :: Board -> Agent -> Object -> PathCalcType -> [Action Position]
findObject board ag targetx calcType =
  let paths = pathToTask board ag targetx calcType
      (bestPath, _) = foldl' takeMin ([], Infinite) paths
   in reverse bestPath
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
