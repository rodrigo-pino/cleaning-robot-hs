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
      (boardByWorld, newG) =
        if times `mod` shuffleT == 0
          then worldSim boardByAgents g
          else (boardByAgents, g)
      nextAgents =
        if times `mod` shuffleT == 0 && agentType == Bold
          then agentInit boardByWorld
          else updatedAgents
   in do
        simOutput boardByWorld times
        endSim <- endSim board
        print updatedAgents
        print (times `mod` shuffleT)
        if not endSim
          then simulation boardByWorld nextAgents (times + 1) shuffleT newG agentType calcType
          else print "Simulation Ended"

simOutput board times = do
  print ("Time: " ++ show times)
  -- print "Agent move:"
  -- printState boardByAgents
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
