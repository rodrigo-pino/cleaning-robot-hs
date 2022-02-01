module Agent.Logic.Pathfinding.PathCalculation where

import Agent.Objects
import World.Objects

data PathCalcType = ShortestPath | BestReward

actionCalc :: PathCalcType -> Board -> Natural -> Action Position -> Natural
actionCalc typex board acc move =
  case typex of
    ShortestPath -> acc + 1
    BestReward -> error "Not yet implemented"
