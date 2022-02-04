module ParallelPathfindingSpec where

import Agent.Logic.Pathfinding.Find
import Agent.Logic.Pathfinding.PathCalculation
import Agent.Logic.TaskHandling (getTasks)
import Agent.Objects
import Agent.Simulate (moveAgents, moveAgentsOld)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Test.Hspec
import Test.Tasty
import Test.Tasty.Hspec
import World.Board
import World.Objects

parallelPathfindingSpec = describe "Benchmark parallel vs sequential algorithms" $ do
  it "Compare parallel vs sequential pathfinding" $
    let robots = makeMany (Robot Nothing) [(0, i) | i <- [0 .. 30]]
        kids = makeMany Kid [(15, i) | i <- [0 .. 30]]
        dirts = makeMany Dirt [(2 * j + 1, i) | i <- [0 .. 30], j <- [1 .. 10]]
        cribs = makeMany Crib [(30, i) | i <- [0 .. 30]]
        board = newBoard 40 40 *++ (robots ++ kids ++ dirts ++ cribs)
        agents = [Agent rob Nothing | rob <- robots]
        targets = cribs

        paths = [findObject board agent target calcType | (agent, target) <- zip agents targets]
        pathsPar = findObjects board agents targets calcType
     in do
          print "Solving with parallel execution"
          startPar <- getCurrentTime
          pathsPar `shouldBe` pathsPar
          endPar <- getCurrentTime
          print ("Solved in " ++ show (endPar `diffUTCTime` startPar) ++ " seconds")
          print "Solving sequentially"
          start <- getCurrentTime
          paths `shouldBe` paths
          end <- getCurrentTime
          print ("Solved in " ++ show (end `diffUTCTime` start) ++ " seconds")
          pathsPar `shouldBe` paths

  it "Compare parallel vs sequential solver finding" $
    let robots = makeMany (Robot Nothing) [(0, i) | i <- [0 .. 20]]
        kids = makeMany Kid [(2 * j, i) | i <- [0 .. 20], j <- [1 .. 10]]
        dirts = makeMany Dirt [(2 * j + 1, i) | i <- [0 .. 20], j <- [1 .. 10]]
        cribs = makeMany Crib [(22, i) | i <- [0 .. 20]]
        board = newBoard 30 30 *++ (kids ++ dirts ++ cribs ++ robots)
        agents = [Agent rob Nothing | rob <- robots]
        tasks = getTasks board agents

        solvers = findSolversOld board tasks agents calcType
        solversPar = findSolvers board tasks agents calcType
     in do
          print "Solving with parallel execution"
          startPar <- getCurrentTime
          solversPar `shouldBe` solversPar
          endPar <- getCurrentTime
          print ("Solved in " ++ show (endPar `diffUTCTime` startPar) ++ " seconds")
          print "Solving sequentially"
          start <- getCurrentTime
          solvers `shouldBe` solvers
          end <- getCurrentTime
          print ("Solved in " ++ show (end `diffUTCTime` start) ++ " seconds")
          length solvers `shouldBe` length solversPar
          solvers `shouldBe` solversPar

  it "Comparep parallel vs sequential agent moving and pathfinding" $ do
    let robots = makeMany (Robot Nothing) [(0, i) | i <- [0 .. 30]]
        kids = makeMany Kid [(15, i) | i <- [0 .. 30]]
        dirts = makeMany Dirt [(2 * j + 1, i) | i <- [0 .. 30], j <- [1 .. 10]]
        cribs = makeMany Crib [(30, i) | i <- [0 .. 30]]
        board = newBoard 40 40 *++ (robots ++ kids ++ dirts ++ cribs)
        agents = [Agent rob (Just (AssignedTask crib [])) | (rob, crib) <- zip robots cribs]

        agMoves = moveAgentsOld board [] agents calcType
        agMovesPar = moveAgents board [] agents calcType
     in do
          print "Solving with parallel execution"
          startPar <- getCurrentTime
          agMovesPar `shouldBe` agMovesPar
          endPar <- getCurrentTime
          print ("Solved in " ++ show (endPar `diffUTCTime` startPar) ++ " seconds")
          print "Solving sequentially"
          start <- getCurrentTime
          agMoves `shouldBe` agMoves
          end <- getCurrentTime
          print ("Solved in " ++ show (end `diffUTCTime` start) ++ " seconds")
          agMoves `shouldBe` agMovesPar

calcType = ShortestPath
