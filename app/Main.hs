module Main where

import Agent.Logic.Pathfinding.PathCalculation
import Agent.Objects
import Simulation

main :: IO ()
main = do
  -- runSimulation 0 5 Bold ShortestPath
  -- runSimulation 1 1 Bold Balance
  -- runSimulation 1 10 Cautious ShortestPath
  -- runSimulation 2 5 Bold Balance
  runSimulation 3 5 Bold (BalanceCrib Nothing)
  print "Exit"
