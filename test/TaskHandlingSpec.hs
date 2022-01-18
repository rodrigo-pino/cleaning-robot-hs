module TaskHandlingSpec where

import Agent.Logic.Pathfinding hiding (make)
import Agent.Logic.TaskHandling
import Agent.Objects
import Data.Matrix as M
import Test.Hspec
import Test.Tasty
import Test.Tasty.Hspec
import World.Board
import World.Objects

findTaskTest = describe "Detect all tasks on board" $ do
  it "Should detect a cleaning task" $
    let dirt = make Dirt (5, 5)
     in getTasks (board *+ dirt) [] `shouldBe` [Task {target = dirt, solvers = []}]
  it "Should detect a crib task" $
    let crib = make Crib (5, 5)
     in getTasks (board *+ crib) [] `shouldBe` [Task {target = crib, solvers = []}]
  it "Should not detect already assigned tasks" $
    let crib = make Crib (5, 5)
        agentx = Agent (make (Robot Nothing) (0, 0)) (Just (AssignedTask crib []))
     in getTasks (board *+ crib) [agentx] `shouldBe` []
  where
    board = newBoard 10 10

taskToMatrixTest =
  describe "Transform tasks and agents input into a cost matrix" $ do
    it "Should produce the correct matrix #1" $
      let (_, tasks, agents) = testData 1
          expected = mockInitialMatrix 3 1 [4, 3, 3] [(0, 0), (1, 0), (2, 0)]
       in getCostMatrix tasks agents `shouldBe` expected
    it "Should produce the correct matrix #2" $
      let (_, tasks, agents) = testData 2
          expected =
            mockInitialMatrix
              3
              2
              [6, 6, 5, 9, 9, 5]
              [(i, j) | i <- [0 .. 2], j <- [0 .. 1]]
       in getCostMatrix tasks agents `shouldBe` expected
    it "Should produce the correct matrix #3" $
      let (_, tasks, agents) = testData 3
          expected =
            mockInitialMatrix
              3
              2
              [5, -1, 5, -1, -1, 5]
              [(i, j) | i <- [0 .. 2], j <- [0 .. 1]]
       in getCostMatrix tasks agents `shouldBe` expected
  where
    mockInitialMatrix :: Int -> Int -> [Int] -> [(Int, Int)] -> M.Matrix (Natural, [(Int, Int)])
    mockInitialMatrix r c vals inds = M.fromList r c (zip (naturals vals) (map (: []) inds))

minimumCostTest :: SpecWith ()
minimumCostTest =
  describe "Find the minimum value in a cost matrix" $ do
    it "Should return the correct minimum #1" $
      let (_, tasks, agents) = testData 1
          initialCostMatrix = getCostMatrix tasks agents
       in do
            minimumCost initialCostMatrix (-1, -1) `shouldBe` (Natural 3, (1, 0))
            minimumCost initialCostMatrix (1, 0) `shouldBe` (Infinite, (-1, -1))
    it "Should return the correct minimum #2" $
      let costMatrix =
            mockMatrix
              3
              3
              [3, 4, 14, 8, 6, 3, 5, 6, 12]
              [ [(0, 0), (1, 2)],
                [(0, 1), (1, 2)],
                [(2, 0), (1, 1)],
                [(1, 0), (0, 1)],
                [(1, 1), (0, 0)],
                [(1, 2), (0, 0)],
                [(2, 0), (1, 2)],
                [(2, 1), (1, 2)],
                [(2, 2), (0, 0)]
              ]
       in do
            minimumCost costMatrix (-1, -1) `shouldBe` (Natural 3, (0, 0))
            minimumCost costMatrix (0, 0) `shouldBe` (Natural 6, (2, 1))
  where
    mockMatrix :: Int -> Int -> [Int] -> [[(Int, Int)]] -> M.Matrix (Natural, [(Int, Int)])
    mockMatrix r c vals inds = M.fromList r c (zip (naturals vals) inds)

optimizationTest =
  describe "Find the optimum task division" $ do
    it "Should return the best optimum #1" $
      getOptimum 1 `shouldBe` [(1, 0)]
    it "Should return the best optimum #2" $
      getOptimum 2 `shouldBe` [(1, 0), (2, 1)]
    it "Should return the best optimum #1" $
      getOptimum 3 `shouldBe` [(0, 0), (2, 1)]
  where
    getOptimum num =
      let (_, tasks, agents) = testData num
       in optimize (getCostMatrix tasks agents)

matrixToTaskTest =
  describe "Transfrom costMatrix into agents with assigned tasks" $ do
    it "Should return the correct agent" $
      3 `shouldBe` 3

testData :: Int -> (Board, [Task], [Agent])
testData num
  | num == 1 =
    let robot = make (Robot Nothing) (0, 0)
        kid = make Kid (1, 1)
        crib = make Crib (2, 2)
        dirt = makeMany Dirt [(0, 2), (2, 0)]
        board = newBoard 3 3 *++ (robot : kid : crib : dirt)
        agent = Agent robot Nothing
        tasks = localHandleTasks board [agent]
     in (board, tasks, [agent])
  | num == 2 =
    let robot1 = make (Robot Nothing) (0, 0)
        robot2 = make (Robot Nothing) (0, 4)
        kid = make Kid (2, 2)
        crib = make Crib (4, 2)
        dirt = makeMany Dirt [(4, 0), (4, 4)]
        agents = [Agent robot1 Nothing, Agent robot2 Nothing]
        board = newBoard 5 5 *++ (robot1 : robot2 : kid : crib : dirt)
        tasks = localHandleTasks board agents
     in (board, tasks, agents)
  | num == 3 =
    let robot1 = make (Robot Nothing) (0, 0)
        robot2 = make (Robot Nothing) (0, 4)
        kid = make Kid (2, 1)
        crib = make Crib (4, 1)
        dirt = makeMany Dirt [(4, 0), (4, 4)]
        obstacles = makeMany Obstacle [(i, 2) | i <- [0 .. 4]]
        agents = [Agent robot1 Nothing, Agent robot2 Nothing]
        board = newBoard 5 5 *++ ([robot1, robot2, kid, crib] ++ obstacles ++ dirt)
        tasks = localHandleTasks board agents
     in (board, tasks, agents)
  | otherwise = error "Invalid test data"
  where
    localHandleTasks board agents = findSolvers board (getTasks board agents) agents

naturals :: [Int] -> [Natural]
naturals = map natural

natural :: Int -> Natural
natural val = if val >= 0 then Natural val else Infinite
