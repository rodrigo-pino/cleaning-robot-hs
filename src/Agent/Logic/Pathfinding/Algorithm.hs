module Agent.Logic.Pathfinding.Algorithm where

import Agent.Logic.Pathfinding.PathCalculation
import Agent.Objects
import Data.HashMap.Strict (HashMap (..), member)
import qualified Data.HashMap.Strict as Map
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

reachableTasks :: Board -> Agent -> PathCalcType -> [ReachedTask]
reachableTasks board agentx calcType =
  let allActions = searchAll calcType initialQueue Map.empty
   in actionToTasks board allActions
  where
    -- queue has initial moves at first
    initialQueue =
      Seq.fromList
        [ (obj, mov, modBoard, initialCost)
          | mov <- moves obj modBoard
        ]

    -- accounting for task reachibility for both robots on a mission and not
    (obj, initialCost, modBoard) = case task agentx of
      Nothing -> (entity agentx, 0, board)
      Just assignedTask ->
        let newPos = (position . destinaton) assignedTask
            newTime = pathCalc calcType board (actions assignedTask)
            oldObj = entity agentx
            newObj = Object (Robot Nothing) newPos
            newBoard = (board *-- [destinaton assignedTask, oldObj]) *+ newObj
         in (newObj, newTime, newBoard)

searchAll :: PathCalcType -> SeqAct -> HashPath -> HashPath
searchAll _ Seq.Empty hashmap = hashmap
searchAll calcType ((obj, act, board, cost) :<| queue) hashmap
  | isJust maybeOldCost =
    let oldCost = fromJust maybeOldCost
     in if newCost < oldCost
          then searchAll calcType newQueue newHashmap
          else searchAll calcType queue hashmap
  | otherwise = searchAll calcType newQueue newHashmap
  where
    -- Logic
    (newObj, newBoard) = localApplyMove board obj act
    newCost = actionCalc calcType board cost act
    newHashmap = Map.insert key newCost hashmap
    nextMoves = moves newObj newBoard
    newQueue = foldl (\acc val -> acc :|> (newObj, val, newBoard, newCost)) queue nextMoves

    -- Helpers
    maybeOldCost = Map.lookup (act, objType) hashmap
    key = (act, objType)
    objType = typex obj

pathToTask :: Board -> Agent -> Object -> PathCalcType -> PossiblePaths
pathToTask board agentx targetx calcType =
  let obj = entity agentx
      initialQueue =
        Seq.fromList
          [(obj, mov, board, [], actionCalc calcType board 0 mov) | mov <- moves obj board]
   in pathfind calcType targetx initialQueue Map.empty

pathfind :: PathCalcType -> Object -> SeqActMem -> HashPath -> PossiblePaths
pathfind _ _ Seq.Empty _ = []
pathfind calcType targetx ((obj, act, board, path, cost) :<| queue) hashmap
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
      | completedTask = pathfind calcType targetx queue newHashmap
      | isJust maybeOldCost =
        let oldCost = fromJust maybeOldCost
         in if cost < oldCost
              then pathfind calcType targetx newQueue newHashmap
              else pathfind calcType targetx queue hashmap
      | otherwise = pathfind calcType targetx newQueue newHashmap

    -- Updating vars
    (newObj, newBoard) = localApplyMove board obj act
    newCost = actionCalc calcType board cost act
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

pathCalc :: PathCalcType -> Board -> [Action Position] -> Natural
pathCalc calcType board = foldl (actionCalc calcType board) 0
