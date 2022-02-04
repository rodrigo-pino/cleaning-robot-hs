module Simulation where

import Agent.Logic.Pathfinding.PathCalculation
import Agent.Objects
import Agent.Simulate
import BoardBox
import Debug.Trace (trace)
import System.Random (StdGen, mkStdGen)
import Visual
import World.Board
import World.Objects
import World.Simulate

runSimulation :: Int -> Int -> AgentType -> PathCalcType -> IO ()
runSimulation simSelect shuffleTime agentType calcType =
  let selectedBoard = boardSelect simSelect
      initalAgents = agentInit selectedBoard
      duration = 1
      g = mkStdGen simSelect
   in do
        simOutput selectedBoard duration
        simulation selectedBoard initalAgents duration shuffleTime g agentType calcType

simulation :: Board -> [Agent] -> Int -> Int -> StdGen -> AgentType -> PathCalcType -> IO ()
simulation board agents times shuffleT g agentType calcType =
  let (boardByAgents, updatedAgents) = agentSim calcType board agents
      timeToVar = times `mod` shuffleT
      (boardByWorld, newG) =
        if timeToVar == 0
          then worldSim boardByAgents g
          else (boardByAgents, g)
      nextAgents =
        if timeToVar == 0 && agentType == Cautious
          then agentInit boardByWorld
          else updatedAgents
   in do
        print (if timeToVar == 0 then "World moves" else show timeToVar)
        simOutput boardByWorld times
        endSim <- endSim boardByWorld
        if not endSim
          then simulation boardByWorld nextAgents (times + 1) shuffleT newG agentType calcType
          else print "Simulation Ended"

simOutput board times = do
  print ("Time: " ++ show times)
  print ("Dirtiness: " ++ show (calculateDirtiness board) ++ "%")
  printState board
  print ""

endSim :: Board -> IO Bool
endSim board
  | calculateDirtiness board >= 60 =
    do
      print "House got 60% dirty"
      return True
  | allKidsInCribs board =
    do
      print "All kids in cribs"
      return True
  | otherwise = return False
