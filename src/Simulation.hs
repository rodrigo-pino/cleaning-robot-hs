module Simulation where

import Agent.Objects
import Agent.Simulate
import System.Random (StdGen, mkStdGen)
import Visual
import World.Board
import World.Objects
import World.Simulate

runSimulation :: Int -> Int -> Int -> IO ()
runSimulation simSelect duration shuffleTime =
  let selectedBoard = boardSelect simSelect
      initalAgents = agentInit selectedBoard
      g = mkStdGen simSelect
   in do
        simulation selectedBoard initalAgents duration shuffleTime g

simulation :: Board -> [Agent] -> Int -> Int -> StdGen -> IO ()
simulation board _ _ 0 _ = printState board
simulation board agents times shuffleT g =
  let (boardByAgents, updatedAgents) = agentSim board agents
      (boardByWorld, newG) = worldSim boardByAgents g False
   in do
        print ("Time: " ++ show times)
        print "Agent move:"
        printState boardByAgents
        print "World move:"
        printState boardByWorld
        print ""
        simulation boardByWorld updatedAgents (times - 1) shuffleT newG

boardSelect :: Int -> Board
boardSelect num =
  case num of
    0 ->
      let robot = make (Robot Nothing) (4, 2)
          kids = makeMany Kid [(2, 0), (2, 4)]
          obstacles =
            makeMany
              Obstacle
              [ (1, 0),
                (2, 1),
                (3, 0),
                (1, 4),
                (2, 3),
                (3, 4)
              ]
          cribs = makeMany Crib [(0, 0), (0, 4)]
          objects = robot : (kids ++ obstacles ++ cribs)
       in worldInit 5 5 objects
    1 -> worldInit 10 10 []
    2 -> worldInit 10 10 []
    _ -> error ("No board defined for num " ++ show num)
