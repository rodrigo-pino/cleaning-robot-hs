module ParallellPathfindingSpec where

import Agent.Logic.Pathfinding.Find
import Agent.Logic.Pathfinding.PathCalculation
import Agent.Objects
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Test.Hspec
import Test.Tasty
import Test.Tasty.Hspec
import World.Board
import World.Objects

taskDetectionTest :: SpecWith ()
taskDetectionTest =
  describe "Testing that tasks are updated accordingly when new solvers are found" $ do
    it "Agents should detect all cleaning tasks" $
      let robot = make (Robot Nothing) (10, 15)
          agent = Agent robot Nothing
          dirty = makeMany Dirt [(10, 0), (29, 16)]
          board = (newBoard 30 30 *+ robot *++ dirty)
          tasks = map (objToTask []) dirty

          solvers = findSolvers board tasks [agent] calcType
          solversPar = findSolversPar board tasks [agent] calcType
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

          solvers = findSolvers board tasks [agent1, agent2] calcType
          solversPar = findSolversPar board tasks [agent1, agent2] calcType
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
          solvers = findSolvers board tasks agents calcType
          solversPar = findSolversPar board tasks agents calcType
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
          solvers = findSolvers board tasks agents calcType
          solversPar = findSolversPar board tasks agents calcType
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
          solvers = findSolvers board tasks agents calcType
          solversPar = findSolversPar board tasks agents calcType
          expected =
            [ objToTask (zipWith (curry agnToSolver) agents [5, 4, 3, 2]) d1,
              objToTask (zipWith (curry agnToSolver) agents [6, 5, 4, 3]) d2,
              objToTask (zipWith (curry agnToSolver) agents [7, 6, 5, 4]) d3,
              objToTask (zipWith (curry agnToSolver) agents [8, 7, 6, 5]) d4
            ]
       in do
            solvers `shouldBe` expected
            solversPar `shouldBe` expected
    it "Comparing parallel execution vs non paralell" $
      let robots = makeMany (Robot Nothing) [(0, i) | i <- [0 .. 20]]
          kids = makeMany Kid [(2 * j, i) | i <- [0 .. 20], j <- [1 .. 10]]
          dirts = makeMany Dirt [(2 * j + 1, i) | i <- [0 .. 20], j <- [1 .. 10]]
          cribs = makeMany Crib [(22, i) | i <- [0 .. 20], j <- [0 .. 10]]
          board = newBoard 30 30 *++ (kids ++ dirts ++ cribs ++ robots)
          tasks = map (objToTask []) (dirts ++ cribs)
          agents = [Agent rob Nothing | rob <- robots]

          solvers = findSolvers board tasks agents calcType
          solversPar = findSolversPar board tasks agents calcType
       in do
            print "Solving with parallel execution"
            startPar <- getCurrentTime
            solversPar `shouldBe` solversPar
            endPar <- getCurrentTime
            print ("Solverd in " ++ show (endPar `diffUTCTime` startPar) ++ " seconds")
            print "Solving without parallel execution"
            start <- getCurrentTime
            solvers `shouldBe` solvers
            end <- getCurrentTime
            print ("Solverd in " ++ show (end `diffUTCTime` start) ++ " seconds")
  where
    objToTask slv obj = Task obj slv
    agnToSolver (agn, time) = Solver agn time
    baseBoard = newBoard 5 5

parallelPathfindingSpec = describe "Test parallell pathfinding" $ do
  it "Compare paralles vs sequential pathfinding" $
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
          print ("Solverd in " ++ show (endPar `diffUTCTime` startPar) ++ " seconds")
          print "Solving without parallel execution"
          start <- getCurrentTime
          paths `shouldBe` paths
          end <- getCurrentTime
          print ("Solverd in " ++ show (end `diffUTCTime` start) ++ " seconds")
          pathsPar `shouldBe` paths

calcType = ShortestPath
