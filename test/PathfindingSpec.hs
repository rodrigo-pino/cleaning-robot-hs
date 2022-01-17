module PathfindingSpec where

import Agent.Logic.Pathfinding hiding (make)
import Agent.Objects
import Test.Hspec
import Test.Tasty
import Test.Tasty.Hspec
import World.Board
import World.Objects

taskReachabilityTest :: SpecWith ()
taskReachabilityTest = describe "Testing agent ability to detect tasks to be done" $ do
  it "Should reach all adyacent cleaning tasks" $
    let robot = make (Robot Nothing) (0, 0)
        agent = Agent robot Nothing
        dirty = makeMany Dirt [(1, 0), (0, 1), (1, 1)]
        board = newBoard 2 2 *+ robot *++ dirty
        reachable = reachableTasks board agent
        expected = [(make Dirt (1, 0), Natural 2), (make Dirt (0, 1), Natural 2), (make Dirt (1, 1), Natural 3)]
     in reachable `shouldBe` expected
  it "Should reach all cleaning tasks" $
    let robot = make (Robot Nothing) (2, 2)
        agent = Agent robot Nothing
        dirty = makeMany Dirt [(2, 0), (4, 4)]
        board = baseBoard *+ robot *++ dirty
        reachable = reachableTasks board agent
        expected = [(make Dirt (2, 0), Natural 3), (make Dirt (4, 4), Natural 5)]
     in reachable `shouldBe` expected
  it "Should reach the crib task" $
    let robot = make (Robot (Just Kid)) (0, 0)
        agent = Agent robot Nothing
        crib = make Crib (0, 2)
        board = baseBoard *++ [robot, crib]
        reachable = reachableTasks board agent
        expected = [(make Crib (0, 2), Natural 2)]
     in reachable `shouldBe` expected
  it "Should reach the crib and cleaning tasks" $
    let robot = make (Robot Nothing) (0, 0)
        agent = Agent robot Nothing
        crib = make Crib (0, 3)
        dirt = make Dirt (1, 3)
        kids = makeMany Kid [(1, 0), (0, 1), (1, 1), (0, 2), (1, 2)]
        board = newBoard 2 4 *++ (robot : crib : dirt : kids)
        reachable = reachableTasks board agent
        expected = [(make Crib (0, 3), Natural 5), (make Dirt (1, 3), Natural 7)]
     in reachable `shouldBe` expected
  it "Should not reach crib task since it cannot reach kid" $
    let robot = make (Robot Nothing) (0, 0)
        agent = Agent robot Nothing
        crib = make Crib (0, 1)
        kid = make Kid (0, 3)
        obstacles = makeMany Obstacle [(0, 2), (1, 2)]
        board = newBoard 2 4 *++ (robot : crib : kid : obstacles)
        reachable = reachableTasks board agent
        expected = []
     in reachable `shouldBe` expected
  where
    baseBoard = newBoard 5 5

taskDetectionTest :: SpecWith ()
taskDetectionTest =
  describe "Testing that tasks are updated accordingly when new solvers are found" $ do
    it "Agents should detect all cleaning tasks" $
      let robot = make (Robot Nothing) (10, 15)
          agent = Agent robot Nothing
          dirty = makeMany Dirt [(10, 0), (29, 16)]
          board = newBoard 30 30 *+ robot *++ dirty
          tasks = map (objToTask []) dirty
       in findSolvers board tasks [agent]
            `shouldBe` [ objToTask [Solver agent (Natural 16)] (make Dirt (10, 0)),
                         objToTask [Solver agent (Natural 21)] (make Dirt (29, 16))
                       ]
    it "Each agent should detect the crib task" $
      let robot1 = make (Robot Nothing) (0, 0)
          robot2 = make (Robot Nothing) (0, 4)
          agent1 = Agent robot1 Nothing
          agent2 = Agent robot2 Nothing
          crib = make Crib (4, 2)
          kid = make Kid (0, 2)
          board = newBoard 5 5 *++ [robot1, robot2, crib, kid]
          tasks = [objToTask [] crib]
       in findSolvers board tasks [agent1, agent2]
            `shouldBe` [objToTask [Solver agent1 (Natural 5), Solver agent2 (Natural 5)] crib]
  where
    objToTask slv obj = Task obj slv
    baseBoard = newBoard 5 5

agentPathfindingTest :: SpecWith ()
agentPathfindingTest = describe "Test agent correct pathfinding to assigned target" $ do
  it "Agent should find the shortest path to it's target task" $
    let robot = make (Robot Nothing) (0, 0)
        agent = Agent robot Nothing
        dirt = make Dirt (2, 2)
        board = newBoard 3 3 *++ [robot, dirt]
        pathCalc = length (pathToTask board agent dirt)
        expected = 5
     in pathCalc `shouldBe` expected
  it "Agent should grab kid to arrive faster" $
    let robot = make (Robot Nothing) (0, 0)
        agent = Agent robot Nothing
        kid = make Kid (0, 1)
        dirt = make Dirt (14, 0)
        board = newBoard 15 2 *++ [robot, kid, dirt]
        pathCalc = length (pathToTask board agent dirt)
        expected = 11
     in pathCalc `shouldBe` expected
