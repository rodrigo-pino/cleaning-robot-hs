module TaskHandlingSpec where

import Agent.Logic.Pathfinding.Find
import Agent.Logic.Pathfinding.PathCalculation (PathCalcType (ShortestPath))
import Agent.Logic.TaskHandling
import Agent.Objects
import Data.List (intersect, sort)
import qualified Data.Matrix as M
import Data.Maybe (fromJust, isNothing, mapMaybe)
import Debug.Trace (trace)
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
    it "Should produce the correct matrix #6" $
      let (_, tasks, agents) = testData 6
          expected =
            mockInitialMatrix
              4
              4
              (concat [[i - j | j <- [0 .. 3]] | i <- [5 .. 8]])
              [(i, j) | i <- [0 .. 3], j <- [0 .. 3]]
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
    it "Should return the best optimum #3" $
      getOptimum 3 `shouldBe` [(0, 0), (2, 1)]
    it "Should return the best optimum #4" $
      sort (getOptimum 4) `shouldBe` [(0, 0), (1, 1), (2, 2)]
    it "Should return the best optimum #5" $
      sort (getOptimum 5) `shouldBe` [(i, i) | i <- [0 .. 2]]
  where
    getOptimum num =
      let (_, tasks, agents) = testData num
       in optimize (getCostMatrix tasks agents)

matrixToTaskTest =
  describe "Transfrom costMatrix into agents with assigned tasks" $ do
    it "Should return the correct agent #1" $
      let (board, tasks, [agent]) = testData 1
          optimum = optimize (getCostMatrix tasks [agent])
          result = parseTaskDivision board tasks [agent] optimum calcType
       in result
            `shouldBe` [ mockAgent
                           agent
                           (make Dirt (4, 0))
                           ([Move (i, 0) | i <- [0 .. 4]] ++ [Clean (4, 0)])
                       ]
    it "Should return the correct agent #2" $
      let (board, tasks, [agent1, agent2]) = testData 2
          optimum = optimize (getCostMatrix tasks [agent1, agent2])
          result = parseTaskDivision board tasks [agent1, agent2] optimum calcType
       in result
            `shouldBe` [ mockAgent
                           agent1
                           (make Dirt (4, 0))
                           ([Move (i, 0) | i <- [0 .. 4]] ++ [Clean (4, 0)]),
                         mockAgent
                           agent2
                           (make Dirt (4, 4))
                           ([Move (i, 4) | i <- [0 .. 4]] ++ [Clean (4, 4)])
                       ]
  where
    mockAgent :: Agent -> Object -> [Action (Int, Int)] -> Agent
    mockAgent agent obj actions = Agent (entity agent) (Just (AssignedTask obj (makeAct actions)))
    makeAct :: [Action (Int, Int)] -> [Action Position]
    makeAct (x : xs) =
      let act = case x of
            (Clean pos) -> Clean (positionFromTuple pos)
            (Move pos) -> Move (positionFromTuple pos)
            (Grab pos) -> Grab (positionFromTuple pos)
            (Drop pos) -> Drop (positionFromTuple pos)
       in act : makeAct xs

assignTaskTest = describe "Correct assignation of tasks to agents" $ do
  it "Should do nothing when all agents are assigned to task" $
    let dirts = makeMany Dirt [(0, i * 2) | i <- [0 .. 2]]
        robots = makeMany (Robot Nothing) [(4, i * 2) | i <- [0 .. 2]]
        moves = [Move (Position 3 (i * 2)) | i <- [0 .. 2]]
        board = newBoard 5 5 *++ (dirts ++ robots)
        agents =
          [ mockAgent ent [mov] target
            | (ent, mov, target) <- zip3 robots moves dirts
          ]
        assigned = assignTasks board agents calcType
     in assigned `shouldBe` agents
  it "Should assign different tasks to each agent" $
    let dirts@[dirt1, dirt2, dirt3] = makeMany Dirt [(4, 0), (0, 4), (2, 2)]
        robots = makeMany (Robot Nothing) [(6, i * 2) | i <- [0 .. 2]]
        board = newBoard 7 5 *++ (dirts ++ robots)
        agents = [mockAgent ent [] ent | ent <- robots]
        resultAgents = assignTasks board agents calcType
        resultDest = map (destinaton . fromJust . task) resultAgents
     in length (resultDest `intersect` dirts) `shouldBe` length dirts
  it "Should assign tasks with a path through a narrow corridor" $
    let obstacles = makeMany Obstacle [(i, j) | i <- [1 .. 4], j <- [0, 1, 3, 4]]
        dirts@[dirt1, dirt2, dirt3] = makeMany Dirt [(0, j) | j <- [0, 2, 4]]
        robots = makeMany (Robot Nothing) [(5, 0), (5, 2), (5, 4)] -- [(5, j) | j <- [0, 2, 4]]
        board = newBoard 6 5 *++ (obstacles ++ dirts ++ robots)
        agents = [Agent rob Nothing | rob <- robots]
        resultAgents = assignTasks board agents calcType
        resultDest = map destinaton (mapMaybe task resultAgents)
     in length (resultDest `intersect` dirts) `shouldBe` 3
  it "Should unassign agents with mission already completed" $
    let dirt = make Dirt (0, 1)
        robot = make (Robot Nothing) (0, 0)
        board = newBoard 3 3 *+ robot
        ag = mockAgent robot [Move (Position 0 1), Clean (Position 0 1)] dirt
        [resAg] = assignTasks board [ag] calcType
     in isNothing (task resAg) `shouldBe` True
  where
    mockAgent :: Object -> [Action Position] -> Object -> Agent
    mockAgent obj [] _ = Agent obj Nothing
    mockAgent obj actions targetx = Agent obj (Just (AssignedTask targetx actions))

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
  | num == 4 =
    let dirts = makeMany Dirt [(4, 0), (2, 2), (0, 4)]
        robots = makeMany (Robot Nothing) [(6, i * 2) | i <- [0 .. 2]]
        board = newBoard 7 5 *++ (dirts ++ robots)
        agents = [Agent ent Nothing | ent <- robots]
        tasks = localHandleTasks board agents
     in (board, tasks, agents)
  | num == 5 =
    let obstacles = makeMany Obstacle [(i, j) | i <- [1 .. 4], j <- [0, 1, 3, 4]]
        dirts@[dirt1, dirt2, dirt3] = makeMany Dirt [(0, j) | j <- [0, 2, 4]]
        robots = makeMany (Robot Nothing) [(5, j) | j <- [0, 2, 4]]
        board = newBoard 6 5 *++ (obstacles ++ dirts ++ robots)
        agents@[ag1, ag2, ag3] = [Agent rob Nothing | rob <- robots]
        tasks = localHandleTasks board agents
     in (board, tasks, agents)
  | num == 6 =
    let robots = makeMany (Robot Nothing) [(0, i) | i <- [0 .. 3]]
        dirts = makeMany Dirt [(0, i) | i <- [4 .. 7]]
        board = newBoard 1 8 *++ (robots ++ dirts)
        agents = [Agent rob Nothing | rob <- robots]
        tasks = localHandleTasks board agents
     in (board, tasks, agents)
  | otherwise = error "Invalid test data"
  where
    localHandleTasks board agents = findSolvers board (getTasks board agents) agents calcType

naturals :: [Int] -> [Natural]
naturals = map natural

natural :: Int -> Natural
natural val = if val >= 0 then Natural val else Infinite

calcType = ShortestPath
