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
simulation board _ 0 _ _ = simOutput board 0
simulation board agents times shuffleT g =
  let (boardByAgents, updatedAgents) = agentSim board agents
      (boardByWorld, newG) = worldSim boardByAgents g False
   in do
        simOutput boardByWorld times
        simulation boardByWorld updatedAgents (times - 1) shuffleT newG

simOutput board times = do
  print ("Time: " ++ show times)
  -- print "Agent move:"
  -- printState boardByAgents
  print ("Dirtiness: " ++ show (calculateDirtiness board) ++ "%")
  printState board
  print ""

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
    1 ->
      let robots = makeMany (Robot Nothing) [(11, 0), (11, 11)]
          kids = makeMany Kid ([(11, j) | j <- [4 .. 7]] ++ [(0, 5), (0, 6)])
          cribs = makeMany Crib ([(i, j) | i <- [0, 1], j <- [0, 11]] ++ [(0, 2), (0, 9)])
          obstacles =
            makeMany
              Obstacle
              ( [(i, j) | i <- [4, 8], j <- [0, 1, 10, 11]]
                  ++ [(i, j) | i <- [0 .. 4] ++ [8 .. 11], j <- [3, 8]]
              )
       in worldInit 12 12 (robots ++ kids ++ cribs ++ obstacles)
    2 ->
      let board1 = boardSelect 1
          dirts = makeMany Dirt [(6, j) | j <- [0 .. 11]]
       in board1 *++ dirts
    _ -> error ("No board defined for num " ++ show num)
