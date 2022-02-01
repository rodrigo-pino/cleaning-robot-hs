module Main where

import Agent.Logic.Pathfinding.PathCalculation
import Simulation

main :: IO ()
main = runSimulation 1 10 BestReward
