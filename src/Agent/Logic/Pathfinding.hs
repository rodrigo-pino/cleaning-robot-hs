module Agent.Logic.Pathfinding where

import Agent.Objects
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Set (Set, insert, member)
import qualified Data.Set as Set
import World.Board
import World.Objects (Action (..), Board, Object, ObjectType (..), Position)
import qualified World.Objects as WO

type SeqAct = Seq (Object, Action Position, Int)

type SetAct = Set (Action Position)

type ReachedTask = (Object, Int)

findSolvers :: Board -> [Task] -> [Agent] -> [Task]
findSolvers _ tasks [] =
  [ Task {target = targetx, solvers = reverse (solversx)}
    | taskx <- tasks,
      let targetx = target taskx,
      let solversx = solvers taskx,
      not (null solversx)
  ]
findSolvers board tasks (ag : agents) = findSolvers board updatedTasks agents
  where
    foundTasks = reachableTasks board ag
    updatedTasks =
      [ newTask
        | taskx <- tasks,
          let solversx = solvers taskx,
          (obj, time) <- foundTasks,
          let newSolver = Solver ag (Natural time),
          let newTask =
                if target taskx == obj
                  then Task obj (newSolver : solversx)
                  else taskx
      ]

reachableTasks :: Board -> Agent -> [ReachedTask]
reachableTasks board agentx = searchAll board initialQueue Set.empty
  where
    obj = entity agentx
    initialQueue = Seq.fromList [(obj, mov, 1) | mov <- moves obj board]

searchAll :: Board -> SeqAct -> SetAct -> [ReachedTask]
searchAll _ Empty _ = []
searchAll board ((prevObj, act, time) :<| queue) visited
  | act `member` visited = searchAll board queue visited
  | otherwise = case (act, objType) of
    (Clean pos, _) -> (make Dirt pos, time) : searchAll board queue newVisited
    (Drop pos, Crib) -> (make Crib pos, time) : searchAll board queue newVisited
    _ -> searchAll board nextQueue newVisited
  where
    objType = WO.typex prevObj
    newVisited = act `insert` visited
    nextObj = localApplyMove prevObj act
    nextMoves = moves nextObj board
    nextQueue = foldl (\acc val -> acc :|> (nextObj, val, time + 1)) queue nextMoves

localApplyMove :: Object -> Action Position -> Object
localApplyMove obj action =
  case (objType, action) of
    (Robot (Just Kid), Drop actPos) -> make (Robot Nothing) actPos
    (Robot Nothing, Grab actPos) -> make (Robot (Just Kid)) actPos
    (Robot c, Move actPos) -> make (Robot c) actPos
    _ -> error "Local apply to unexpected action or object"
  where
    objType = WO.typex obj

make obj pos = WO.make obj (WO.tupleFromPosition pos)
