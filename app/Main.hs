module Main where

import Agent.Logic.Pathfinding.PathCalculation
import Agent.Objects
import Simulation

main :: IO ()
main = do
  -- runSimulation 0 1 Cautious Balance
  -- runSimulation 0 1 Bold Balance
  -- runSimulation 1 1 Cautious Balance
  -- runSimulation 1 1 Bold Balance
  -- runSimulation 1 10 Cautious ShortestPath
  -- runSimulation 1 10 Bold ShortestPath
  -- runSimulation 2 1 Cautious ShortestPath
  -- runSimulation 2 1 Bold ShortestPath
  -- runSimulation 2 1 Cautious Balance
  -- runSimulation 2 1 Bold Balance
  -- runSimulation 3 5 Bold ShortestPath
  runSimulation 3 5 Cautious (BalanceCrib Nothing)
  print "Exit"
