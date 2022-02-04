module PathfindingSpec where

import Agent.Logic.Pathfinding.Algorithm (pathToTask, reachableTasks)
import Agent.Logic.Pathfinding.Algorithm hiding (make)
import Agent.Logic.Pathfinding.Find (findObject, findSolvers, findSolversOld, findTarget)
import Agent.Logic.Pathfinding.PathCalculation
import Agent.Objects
import Data.List (nub, sort)
import Debug.Trace (trace)
import Test.Hspec
import Test.Tasty
import Test.Tasty.Hspec
import Visual (printState)
import World.Board
import World.Objects

taskReachabilityTest :: SpecWith ()
taskReachabilityTest = describe "Testing agent ability to detect tasks to be done" $ do
  it "Should reach all adyacent cleaning tasks" $
    let robot = make (Robot Nothing) (0, 0)
        agent = Agent robot Nothing
        dirty = makeMany Dirt [(1, 0), (0, 1), (1, 1)]
        board = newBoard 2 2 *+ robot *++ dirty
        reachable = reachableTasks board agent calcType
        expected =
          [ (make Dirt (1, 0), Natural 2),
            (make Dirt (0, 1), Natural 2),
            (make Dirt (1, 1), Natural 3)
          ]
     in sort reachable `shouldBe` sort expected
  it "Should reach all cleaning tasks" $
    let robot = make (Robot Nothing) (2, 2)
        agent = Agent robot Nothing
        dirty = makeMany Dirt [(2, 0), (4, 4)]
        board = baseBoard *+ robot *++ dirty
        reachable = reachableTasks board agent calcType
        expected = [(make Dirt (2, 0), Natural 3), (make Dirt (4, 4), Natural 5)]
     in sort reachable `shouldBe` sort expected
  it "Should reach the crib task" $
    let robot = make (Robot (Just Kid)) (0, 0)
        agent = Agent robot Nothing
        crib = make Crib (0, 2)
        board = baseBoard *++ [robot, crib]
        reachable = reachableTasks board agent calcType
        expected = [(make Crib (0, 2), Natural 2)]
     in sort reachable `shouldBe` sort expected
  it "Should reach the crib and cleaning tasks" $
    let robot = make (Robot Nothing) (0, 0)
        agent = Agent robot Nothing
        crib = make Crib (0, 3)
        dirt = make Dirt (1, 3)
        kids = makeMany Kid [(1, 0), (0, 1), (1, 1), (0, 2), (1, 2)]
        board = newBoard 2 4 *++ (robot : crib : dirt : kids)
        reachable = reachableTasks board agent calcType
        expected = [(make Crib (0, 3), Natural 5), (make Dirt (1, 3), Natural 7)]
     in sort reachable `shouldBe` sort expected
  it "Should not reach crib task since it cannot reach kid" $
    let robot = make (Robot Nothing) (0, 0)
        agent = Agent robot Nothing
        crib = make Crib (0, 1)
        kid = make Kid (0, 3)
        obstacles = makeMany Obstacle [(0, 2), (1, 2)]
        board = newBoard 2 4 *++ (robot : crib : kid : obstacles)
        reachable = reachableTasks board agent calcType
        expected = []
     in sort reachable `shouldBe` sort expected
  it "Should reach a task from a narrow corridor" $
    let obstacles = makeMany Obstacle [(i, j) | i <- [1 .. 4], j <- [0, 1, 3, 4]]
        dirts@[dirt1, dirt2, dirt3] = makeMany Dirt [(0, j) | j <- [0, 2, 4]]
        robots = makeMany (Robot Nothing) [(5, j) | j <- [0, 2, 4]]
        board = newBoard 6 5 *++ (obstacles ++ dirts ++ robots)
        agents@[ag1, ag2, ag3] = [Agent rob Nothing | rob <- robots]
     in do
          reachableTasks board ag1 calcType `shouldBe` []
          sort (reachableTasks board ag2 calcType)
            `shouldBe` sort
              [ (dirt2, Natural 6),
                (dirt3, Natural 8),
                (dirt1, Natural 8)
              ]
          reachableTasks board ag3 calcType `shouldBe` []
  it "Should detect all cleaning tasks in a big board" $
    let robot = make (Robot Nothing) (10, 15)
        agent = Agent robot Nothing
        dirty@[d1, d2] = makeMany Dirt [(10, 0), (29, 16)]
        board = (newBoard 30 30 *+ robot *++ dirty)
     in sort (reachableTasks board agent calcType) `shouldBe` sort [(d1, 16), (d2, 21)]
  where
    baseBoard = newBoard 5 5

taskDetectionTest :: SpecWith ()
taskDetectionTest =
  describe "Testing that tasks are updated accordingly when new solvers are found" $ do
    it "Agents should detect all cleaning tasks" $
      let robot = make (Robot Nothing) (10, 15)
          agent = Agent robot Nothing
          dirty = makeMany Dirt [(10, 0), (29, 16)]
          board = (newBoard 30 30 *+ robot *++ dirty)
          tasks = map (objToTask []) dirty

          solvers = findSolversOld board tasks [agent] calcType
          solversPar = findSolvers board tasks [agent] calcType
          expected =
            [ objToTask [Solver agent (Natural 16)] (make Dirt (10, 0)),
              objToTask [Solver agent (Natural 21)] (make Dirt (29, 16))
            ]
       in do
            solvers `shouldBe` expected
            solversPar `shouldBe` expected
    it "Each agent should detect the crib task" $
      let robot1 = make (Robot Nothing) (0, 0)
          robot2 = make (Robot Nothing) (0, 4)
          agent1 = Agent robot1 Nothing
          agent2 = Agent robot2 Nothing
          crib = make Crib (4, 2)
          kid = make Kid (0, 2)
          board = newBoard 5 5 *++ [robot1, robot2, crib, kid]
          tasks = [objToTask [] crib]

          solvers = findSolversOld board tasks [agent1, agent2] calcType
          solversPar = findSolvers board tasks [agent1, agent2] calcType
          expected = [objToTask [Solver agent1 (Natural 5), Solver agent2 (Natural 5)] crib]
       in do
            solvers `shouldBe` expected
            solversPar `shouldBe` expected
    it "Each agent should detect all tasks" $
      let robot1 = make (Robot Nothing) (0, 0)
          robot2 = make (Robot Nothing) (0, 4)
          crib = make Crib (4, 2)
          kid = make Kid (0, 2)
          dirts@[d1, d2] = makeMany Dirt [(4, 0), (4, 4)]
          board = newBoard 5 5 *++ (robot1 : robot2 : crib : kid : dirts)
          agents = [Agent robot1 Nothing, Agent robot2 Nothing]
          tasks = map (objToTask []) (crib : dirts)
          solvers = findSolversOld board tasks agents calcType
          solversPar = findSolvers board tasks agents calcType
          expected =
            [ objToTask (zipWith (curry agnToSolver) agents [5, 5]) crib,
              objToTask (zipWith (curry agnToSolver) agents [5, 8]) d1,
              objToTask (zipWith (curry agnToSolver) agents [8, 5]) d2
            ]
       in do
            solvers `shouldBe` expected
            solversPar `shouldBe` expected
    it "All agents should find all tasks despite being blocked by other robots" $
      let obstacles = makeMany Obstacle [(i, j) | i <- [1 .. 4], j <- [0, 1, 3, 4]]
          dirts@[dirt1, dirt2, dirt3] = makeMany Dirt [(0, j) | j <- [0, 2, 4]]
          robots = makeMany (Robot Nothing) [(5, j) | j <- [0, 2, 4]]
          board = newBoard 6 5 *++ (obstacles ++ dirts ++ robots)
          agents@[ag1, ag2, ag3] = [Agent rob Nothing | rob <- robots]
          tasks = map (objToTask []) dirts
          solvers = findSolversOld board tasks agents calcType
          solversPar = findSolvers board tasks agents calcType
          expected =
            [ objToTask (zipWith (curry agnToSolver) agents [10, 8, 10]) dirt1,
              objToTask (zipWith (curry agnToSolver) agents [8, 6, 8]) dirt2,
              objToTask (zipWith (curry agnToSolver) agents [10, 8, 10]) dirt3
            ]
       in do
            solvers `shouldBe` expected
            solversPar `shouldBe` expected
    it "All agents in a line should find all tasks despite this" $
      let robots = makeMany (Robot Nothing) [(0, i) | i <- [0 .. 3]]
          dirts@[d1, d2, d3, d4] = makeMany Dirt [(0, i) | i <- [4 .. 7]]
          board = newBoard 1 8 *++ (robots ++ dirts)
          agents = [Agent rob Nothing | rob <- robots]
          tasks = map (objToTask []) dirts
          solvers = findSolversOld board tasks agents calcType
          solversPar = findSolvers board tasks agents calcType
          expected =
            [ objToTask (zipWith (curry agnToSolver) agents [5, 4, 3, 2]) d1,
              objToTask (zipWith (curry agnToSolver) agents [6, 5, 4, 3]) d2,
              objToTask (zipWith (curry agnToSolver) agents [7, 6, 5, 4]) d3,
              objToTask (zipWith (curry agnToSolver) agents [8, 7, 6, 5]) d4
            ]
       in do
            solvers `shouldBe` expected
            solversPar `shouldBe` expected
  where
    objToTask slv obj = Task obj slv
    agnToSolver (agn, time) = Solver agn time
    baseBoard = newBoard 5 5

agentPathfindingTest :: SpecWith ()
agentPathfindingTest = describe "Test agent correct pathfinding to assigned target" $ do
  it "Agent should find the shortest path to it's target task" $
    let robot = make (Robot Nothing) (0, 0)
        agent = Agent robot Nothing
        dirt = make Dirt (2, 2)
        board = newBoard 3 3 *++ [robot, dirt]
        pathCalc = length (findObject board agent dirt calcType)
        expected = 5
     in pathCalc `shouldBe` expected
  it "Agent should grab kid to arrive faster" $
    let robot = make (Robot Nothing) (0, 0)
        agent = Agent robot Nothing
        kid = make Kid (0, 1)
        dirt = make Dirt (14, 0)
        board = newBoard 15 2 *++ [robot, kid, dirt]
        pathCalc = length (findObject board agent dirt calcType)
        expected = 11
     in pathCalc `shouldBe` expected
  it "Should find the path to the far away dirt" $
    let robot = make (Robot Nothing) (0, 4)
        dirts@[d1, d2, d3, d4] = makeMany Dirt [(0, i) | i <- [4 .. 7]]
        board = newBoard 1 8 *++ (robot : dirts)
        ag = Agent robot (Just (AssignedTask d2 []))
        path = findTarget board ag calcType
     in path `shouldBe` [Move (Position 0 5), Clean (Position 0 5)]
  it "Should find the path to the dirt" $
    let obstacles = makeMany Obstacle [(i, j) | i <- [1 .. 4], j <- [0, 1, 3, 4]]
        dirts@[d1, d2, d3] = makeMany Dirt [(0, j) | j <- [0, 2, 4]]
        robots@[r1, r2, r3] = makeMany (Robot Nothing) [(0, 1), (2, 2), (5, 4)]
        board = newBoard 6 5 *++ (obstacles ++ dirts ++ [r1, r2, r3])
        ag = Agent r2 (Just (AssignedTask d3 []))
        path = findTarget board ag calcType
        expPath =
          [ Move (Position 1 2),
            Move (Position 0 2),
            Move (Position 0 3),
            Move (Position 0 4),
            Clean (Position 0 4)
          ]
     in path `shouldBe` expPath
  it "Should find the path to the crib" $
    let robot = make (Robot Nothing) (0, 1)
        kid = make Kid (0, 0)
        cribs@[c1, c2, c3] = makeMany Crib [(0, i) | i <- [2 .. 4]]
        board = newBoard 1 5 *++ (kid : robot : cribs)
        ag = Agent robot Nothing
        path = findObject board ag c3 calcType
        expPath =
          [ Grab (Position 0 0),
            Move (Position 0 2),
            Move (Position 0 4),
            Drop (Position 0 4)
          ]
     in do
          path `shouldBe` expPath

calcType = ShortestPath
