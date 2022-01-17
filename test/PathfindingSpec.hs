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
  where
    baseBoard = newBoard 5 5

taskDetecttionTest :: SpecWith ()
taskDetecttionTest = describe "Testing that tasks are updated accordingly when new solvers are found" $ do
  it "Should detect all cleaning tasks" $
    let robot = make (Robot Nothing) (10, 15)
        agent = Agent robot Nothing
        dirty = makeMany Dirt [(10, 0), (29, 16)]
        board = baseBoard *+ robot *++ dirty
        tasks = map (objToTask []) dirty
        t = reachableTasks board agent
     in do
          print t
          findSolvers board tasks [agent]
            `shouldBe` [ objToTask [Solver agent (Natural 15)] (make Dirt (10, 0)),
                         objToTask [Solver agent (Natural 20)] (make Dirt (29, 16))
                       ]
  it "Should find only the reachable cleaning tasks" $
    3 `shouldBe` 3
  it "Each agent should find only the tasks it has access to" $
    3 `shouldBe` 3
  where
    objToTask slv obj = Task obj slv
    baseBoard = newBoard 5 5
