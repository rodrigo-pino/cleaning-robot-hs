module Agent.Logic.Pathfinding.Algorithm where

import Agent.Logic.Pathfinding.PathCalculation
import Agent.Objects
import Data.HashMap.Strict (HashMap (..), member)
import qualified Data.HashMap.Strict as Map
import Data.HashPSQ (HashPSQ (..))
import qualified Data.HashPSQ as PSQ
import Data.Maybe (fromJust, isJust)
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Debug.Trace (trace)
import GHC.List (foldl')
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

type PQueue = HashPSQ (Object, Action Position) Natural (Board, [Action Position])

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
            newTime = pathCalc calcType board (destinaton assignedTask) (actions assignedTask)
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
    newCost = actionCalc calcType board Nothing cost act
    newHashmap = Map.insert key newCost hashmap
    nextMoves = moves newObj newBoard
    newQueue =
      foldl'
        ( \acc val ->
            newObj
              `seq` val
              `seq` newBoard
              `seq` newCost
              `seq` acc :|> (newObj, val, newBoard, newCost)
        )
        queue
        nextMoves

    -- Helpers
    maybeOldCost = Map.lookup (act, objType) hashmap
    key = (act, objType)
    objType = typex obj

pathToTask :: Board -> Agent -> Object -> PathCalcType -> [Action Position]
pathToTask board agentx targetx calcType =
  let obj = entity agentx
      initialQueue =
        PSQ.fromList
          [ ( (obj, mov),
              actionCalc calcType board (Just targetx) 0 mov,
              (board, [])
            )
            | mov <- moves obj board
          ]
   in pathfind calcType targetx initialQueue Map.empty

pathfind :: PathCalcType -> Object -> PQueue -> HashPath -> [Action Position]
pathfind calcType targetx pqueue hashmap
  | PSQ.null pqueue = []
  | value act == position targetx =
    case act of
      (Clean pos) -> newPath -- [(newPath, newCost)] -- : keepFinding True
      (Drop pos) ->
        if fromJust (board WO.! pos) == [Crib, Robot (Just Kid)]
          then newPath -- [(newPath, newCost)] -- : keepFinding True
          else keepFinding False
      _ -> keepFinding False
  | otherwise = keepFinding False
  where
    -- Logic
    keepFinding completedTask
      | completedTask = pathfind calcType targetx updPqueue newHashmap
      | isJust maybeOldCost =
        let oldCost = fromJust maybeOldCost
         in if newCost < oldCost
              then pathfind calcType targetx newQueue newHashmap
              else pathfind calcType targetx updPqueue hashmap
      | otherwise = pathfind calcType targetx newQueue newHashmap

    -- Updating vars
    (newObj, newBoard) = localApplyMove board obj act
    newCost = actionCalc calcType board (Just targetx) cost act
    newPath = act : path
    newHashmap = Map.insert key newCost hashmap
    nextMoves = moves newObj newBoard
    newQueue =
      foldl'
        ( \acc val ->
            newObj
              `seq` val
              `seq` newCost
              `seq` newBoard
              `seq` newPath
              `seq` PSQ.insert (newObj, val) newCost (newBoard, newPath) acc
        )
        updPqueue
        nextMoves

    -- Extracting vars
    ((obj, act), cost, (board, path)) = fromJust (PSQ.findMin pqueue)
    updPqueue = PSQ.deleteMin pqueue

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
      | Crib `elem` cellP && Kid `notElem` cellP = (make Crib p, t) : loop actions
      | otherwise = loop actions
      where
        cellP = fromJust (board WO.! p)
    loop (_ : actions) = loop actions
    loop [] = []

    allActions = [(act, t) | ((act, _), t) <- Map.toList hashmap]

pathCalc :: PathCalcType -> Board -> Object -> [Action Position] -> Natural
pathCalc calcType board target = foldl' (actionCalc calcType board (Just target)) 0
