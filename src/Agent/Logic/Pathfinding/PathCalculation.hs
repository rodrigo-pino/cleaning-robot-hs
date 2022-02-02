module Agent.Logic.Pathfinding.PathCalculation where

import Agent.Objects
import Data.Maybe (fromJust, isJust)
import Debug.Trace (trace)
import World.Objects

data PathCalcType = ShortestPath | BestReward

actionCalc :: PathCalcType -> Board -> Maybe Object -> Natural -> Action Position -> Natural
actionCalc typex board target acc move =
  case typex of
    ShortestPath -> acc + 1
    BestReward -> bestRewardCalc board acc move target

bestRewardCalc :: Board -> Natural -> Action Position -> Maybe Object -> Natural
bestRewardCalc board acc@(Natural val) move target =
  case move of
    Drop pos
      | fromJust (board ! pos) == [Crib, Robot (Just Kid)] ->
        min
          (Natural (val `div` 2)) -- to reward long paths
          (acc - baseMoveCost * avg) -- to reward short paths
      | otherwise -> acc + baseMoveCost * (1 + 2 * kidsAround pos)
    Grab pos -> acc + baseMoveCost * (1 - 2 * kidsAround pos) + extraCost
    Clean pos -> acc
    Move pos -> acc + baseMoveCost + extraCost
    _ -> error ("Unexpected movement type: " ++ show move)
  where
    -- Helpers
    baseMoveCost = Natural 100
    kidsAround position = Natural (length (filter ([Kid] ==) (adyacentsTo board position)))
    avg = Natural ((maxRows board + maxCols board) `div` 2)
    extraCost =
      let baseVal =
            if isJust target
              then distance ((position . fromJust) target) (value move)
              else 0
       in Natural (baseVal * 10)

distance p1 p2 =
  let r = abs (row p1 - row p2)
      c = abs (col p1 - col p2)
   in r + c
