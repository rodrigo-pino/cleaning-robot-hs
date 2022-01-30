module Agent.Logic.Pathfinding where

import Agent.Objects
import Data.Maybe (fromJust, isJust)
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Set (Set, insert, member)
import qualified Data.Set as Set
import Debug.Trace (trace)
import World.Board
import World.Objects
  ( Action (..),
    Board,
    Object (..),
    ObjectType (..),
    Position,
    position,
    typex,
    value,
    (*+),
    (*--),
  )
import qualified World.Objects as WO

type SeqAct = Seq (Object, Action Position, Board, Natural)

type SeqActMem = Seq (Object, Action Position, Board, [Action Position])

type SetAct = [(Action Position, ObjectType)]

type ReachedTask = (Object, Natural)

findSolvers :: Board -> [Task] -> [Agent] -> [Task]
findSolvers _ tasks [] =
  [ Task {target = targetx, solvers = reverse solversx}
    | taskx <- tasks,
      let targetx = target taskx,
      let solversx = solvers taskx,
      not (null solversx)
  ]
findSolvers board tasks (ag : agents) = findSolvers board updatedTasks agents
  where
    foundTasks = reachableTasks (robotlessBoard board ag) ag
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

reachableTasks :: Board -> Agent -> [ReachedTask]
reachableTasks board agentx = searchAll initialQueue []
  where
    maybeAssignedTask = task agentx
    (obj, time, modBoard) = case maybeAssignedTask of
      Nothing -> (entity agentx, 1, board)
      Just assignedTask ->
        let newPos = (position . destinaton) assignedTask
            newTime = length (actions assignedTask) + 1
            oldObj = entity agentx
            newObj = Object (Robot Nothing) newPos
            newBoard = (board *-- [destinaton assignedTask, oldObj]) *+ newObj
         in (newObj, newTime, newBoard)
    initialQueue = Seq.fromList [(obj, mov, modBoard, Natural time) | mov <- moves obj modBoard]

searchAll :: SeqAct -> SetAct -> [ReachedTask]
searchAll Empty _ = []
searchAll ((prevObj, act, board, time) :<| queue) visited
  | (act, objType) `elem` visited = searchAll queue visited
  | otherwise = case act of
    (Clean pos) -> (make Dirt pos, time) : searchAll queue newVisited
    (Drop pos) ->
      if fromJust (board WO.! pos) == [Crib, Robot (Just Kid)]
        then (make Crib pos, time) : searchAll queue newVisited
        else searchAll nextQueue newVisited
    _ -> searchAll nextQueue newVisited
  where
    objType = typex prevObj
    newVisited = (act, objType) : visited
    (nextObj, nextBoard) = localApplyMove board prevObj act
    nextMoves = moves nextObj nextBoard
    nextTime = time + Natural 1
    nextQueue = foldl (\acc val -> acc :|> (nextObj, val, nextBoard, nextTime)) queue nextMoves

pathToTask :: Board -> Agent -> Object -> [Action Position]
pathToTask board agentx targetx = pathfind targetx initialQueue []
  where
    obj = entity agentx
    initialQueue = Seq.fromList [(obj, mov, board, []) | mov <- moves obj board]

pathfind :: Object -> SeqActMem -> SetAct -> [Action Position]
pathfind _ Empty _ = []
pathfind targetx ((prevObj, act, board, path) :<| queue) visited
  | (act, objType) `elem` visited = pathfind targetx queue visited
  | actPos == tPos =
    case act of
      (Move _) -> pathfind targetx nextQueue newVisited
      _ -> reverse (act : path)
  | otherwise = pathfind targetx nextQueue newVisited
  where
    tPos = position targetx
    actPos = value act
    objType = typex prevObj
    newVisited = (act, objType) : visited
    (nextObj, nextBoard) = localApplyMove board prevObj act
    nextMoves = moves nextObj nextBoard
    nextQueue = foldl (\acc val -> acc :|> (nextObj, val, nextBoard, act : path)) queue nextMoves

localApplyMove :: Board -> Object -> Action Position -> (Object, Board)
localApplyMove board obj action =
  case (objType, action) of
    (Robot (Just Kid), Drop actPos) -> (make (Robot Nothing) actPos, newBoard)
    (Robot Nothing, Grab actPos) -> (make (Robot (Just Kid)) actPos, newBoard)
    (Robot Nothing, Clean actPos) -> (make (Robot Nothing) actPos, newBoard)
    (Robot c, Move actPos) -> (make (Robot c) actPos, newBoard)
    _ -> error "Local apply to unexpected action or object"
  where
    objType = typex obj
    newBoard = applyMove obj action board

make obj pos = WO.make obj (WO.tupleFromPosition pos)

robotlessBoard board ag =
  let newElems = filter f (WO.elems board)
      f obj@(Object (Robot _) pos) = obj == entity ag
      f _ = True
   in WO.Board newElems (WO.maxRows board) (WO.maxCols board)
