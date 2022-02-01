module Agent.Logic.Pathfinding.PathCalculation where

import Agent.Objects
import Data.Maybe (fromJust)
import Debug.Trace (trace)
import World.Objects

data PathCalcType = ShortestPath | BestReward

actionCalc :: PathCalcType -> Board -> Natural -> Action Position -> Natural
actionCalc typex board acc move =
  case typex of
    ShortestPath -> acc + 1
    BestReward -> bestRewardCalc board acc move

bestRewardCalc :: Board -> Natural -> Action Position -> Natural
bestRewardCalc board acc@(Natural val) move =
  case move of
    Drop pos
      | fromJust (board ! pos) == [Crib, Robot (Just Kid)] ->
        min
          (Natural (val `div` 2)) -- to reward long paths
          (acc - baseMoveCost * 5) -- to reward short paths
      | otherwise -> acc + baseMoveCost * (1 + 2 * kidsAround pos)
    Grab pos -> acc + baseMoveCost * (1 - 2 * kidsAround pos)
    Clean pos -> acc - (2 * baseMoveCost)
    Move pos -> acc + baseMoveCost
    _ -> error ("Unexpected movement type: " ++ show move)
  where
    -- Helpers
    baseMoveCost = Natural 100
    kidsAround position = Natural (length (filter ([Kid] ==) (adyacentsTo board position)))
