module Main where

import Simulation
import Agent.Logic.Pathfinding.PathCalculation

main :: IO ()
main = runSimulation 4 100 10 ShortestPath
