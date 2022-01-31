module Agent.Logic.PathfindingNew where

import Agent.Objects
import Data.HashMap (Map (..), member)
import qualified Data.HashMap as Map
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

type ReachedTask = (Object, Natural)

reachableTasks board agentx = searchAll initialQueue Map.empty
  where
    initialQueue =
      Seq.fromList
        [(obj, mov, modBoard, Natural time) | mov <- moves obj modBoard]

    (obj, time, modBoard) = case task agentx of
      Nothing -> (entity agentx, 1, board)
      Just assignedTask ->
        let newPos = (position . destinaton) assignedTask
            newTime = length (actions assignedTask) + 1
            oldObj = entity agentx
            newObj = Object (Robot Nothing) newPos
            newBoard = (board *-- [destinaton assignedTask, oldObj]) *+ newObj
         in (newObj, newTime, newBoard)

searchAll ((obj, act, board, cost) :<| queue) visited hashmap
  | (act, objType) `member` hashmap = 3
  where
    objType = typex obj
