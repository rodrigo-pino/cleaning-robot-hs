module Agent.Logic.PathfindingNew where

import Agent.Objects
import Data.HashMap.Strict (HashMap (..), member)
import qualified Data.HashMap.Strict as Map
import Data.List (nub)
import Data.Maybe (fromJust, isJust)
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
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

type HashPath = HashMap (Action Position, ObjectType) Natural

type ReachedTask = (Object, Natural)

type SeqAct = Seq (Object, Action Position, Board, Natural)

type SeqActMem = Seq (Object, Action Position, Board, [Action Position], Natural)

type PossiblePaths = [([Action Position], Natural)]

reachableTasks :: Board -> Agent -> [ReachedTask]
reachableTasks board agentx =
  let allActions = searchAll initialQueue Map.empty
   in actionToTasks board allActions
  where
    -- queue has initial moves at first
    initialQueue =
      Seq.fromList
        [(obj, mov, modBoard, time) | mov <- moves obj modBoard]

    -- accounting task reachibility both rotbots on a mission and not
    (obj, time, modBoard) = case task agentx of
      Nothing -> (entity agentx, 0, board)
      Just assignedTask ->
        let newPos = (position . destinaton) assignedTask
            newTime = pathCalc board (actions assignedTask)
            oldObj = entity agentx
            newObj = Object (Robot Nothing) newPos
            newBoard = (board *-- [destinaton assignedTask, oldObj]) *+ newObj
         in (newObj, newTime, newBoard)

searchAll :: SeqAct -> HashPath -> HashPath
searchAll Seq.Empty hashmap = hashmap
searchAll ((obj, act, board, cost) :<| queue) hashmap
  | isJust maybeOldCost =
    let oldCost = fromJust maybeOldCost
     in if cost < oldCost
          then searchAll newQueue newHashmap
          else searchAll queue hashmap
  | otherwise = searchAll newQueue newHashmap
  where
    -- Logic
    (newObj, newBoard) = localApplyMove board obj act
    newCost = actionCalc board cost act
    newHashmap = Map.insert key newCost hashmap
    nextMoves = moves newObj newBoard
    newQueue = foldl (\acc val -> acc :|> (newObj, val, newBoard, newCost)) queue nextMoves

    -- Helpers
    maybeOldCost = Map.lookup (act, objType) hashmap
    key = (act, objType)
    objType = typex obj

pathToTask :: Board -> Agent -> Object -> [Action Position]
pathToTask board agentx targetx =
  let obj = entity agentx
      initialQueue =
        Seq.fromList
          [(obj, mov, board, [], actionCalc board 0 mov) | mov <- moves obj board]
      paths = pathfind targetx initialQueue Map.empty

      (bestPath, _) = foldl takeMin ([], Infinite) paths
   in reverse bestPath
  where
    takeMin acc@(_, accCost) val@(_, valCost) =
      if valCost < accCost then val else acc

pathfind :: Object -> SeqActMem -> HashPath -> PossiblePaths
pathfind _ Seq.Empty _ = []
pathfind targetx ((obj, act, board, path, cost) :<| queue) hashmap
  | value act == position targetx =
    case act of
      (Clean pos) -> (act : path, cost) : keepFinding True
      (Drop pos) ->
        if fromJust (board WO.! pos) == [Crib]
          then (act : path, newCost) : keepFinding True
          else keepFinding False
      _ -> keepFinding False
  | otherwise = keepFinding False
  where
    -- Logic
    keepFinding completedTask
      | completedTask = pathfind targetx queue newHashmap
      | isJust maybeOldCost =
        let oldCost = fromJust maybeOldCost
         in if cost < oldCost
              then pathfind targetx newQueue newHashmap
              else pathfind targetx queue hashmap
      | otherwise = pathfind targetx newQueue newHashmap

    -- Updating vars
    (newObj, newBoard) = localApplyMove board obj act
    newCost = actionCalc board cost act
    newHashmap = Map.insert key newCost hashmap
    nextMoves = moves newObj newBoard
    newQueue =
      foldl
        (\acc val -> acc :|> (newObj, val, newBoard, act : path, newCost))
        queue
        nextMoves

    -- Helpers
    maybeOldCost = Map.lookup (act, objType) hashmap
    key = (act, objType)
    objType = typex obj

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

actionToTasks :: Board -> HashPath -> [ReachedTask]
actionToTasks board hashmap = loop allActions
  where
    -- Loop through all actions and select the ones wich corresponds to a task
    loop ((Clean p, t) : actions) = (make Dirt p, t) : loop actions
    loop ((Drop p, t) : actions)
      | fromJust (board WO.! p) == [Crib] = (make Crib p, t) : loop actions
      | otherwise = loop actions
    loop (_ : actions) = loop actions
    loop [] = []

    allActions = [(act, t) | ((act, _), t) <- Map.toList hashmap]

pathCalc :: Board -> [Action Position] -> Natural
pathCalc board = foldl (actionCalc board) 0

actionCalc :: Board -> Natural -> Action Position -> Natural
actionCalc board cost move = cost + 1
