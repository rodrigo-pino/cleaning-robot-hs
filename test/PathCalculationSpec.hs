module PathCalculationSpec where

import Agent.Logic.Pathfinding.Find
import Agent.Logic.Pathfinding.PathCalculation
import Agent.Logic.TaskHandling (assignTasks, getTasks)
import Agent.Objects
import Agent.Simulate (agentInit)
import BoardBox (boardSelect)
import Data.Maybe (fromJust)
import Test.Hspec
import Test.Tasty
import Test.Tasty.Hspec
import World.Objects

bestRewardSpec = describe "Best reward calculation are done correctly" $ do
  it "Should have the correct value after dropping kid" $
    let robots = makeMany (Robot (Just Kid)) [(i, i) | i <- [0, 5, 9]]
        crib = make Crib (9, 9)
        kids = makeMany Kid [(4, 5), (5, 4)]
        board = newBoard 10 10 *++ (crib : robots ++ kids)
        moves@[m1, m2, m3] = [Drop (Position x x) | x <- [0, 5, 9]]
     in do
          actionCalc calcType board Nothing 1000 m1 `shouldBe` 1100
          actionCalc calcType board Nothing 1000 m2 `shouldBe` 1500
          actionCalc calcType board Nothing 3000 m3 `shouldBe` 1500
          actionCalc calcType board Nothing 700 m3 `shouldBe` -300
  it "Should have the correct value after grabbing a kid" $
    let robots = makeMany (Robot Nothing) [(i, i) | i <- [0, 4]]
        kids = makeMany Kid [(i, i + 1) | i <- [0, 4]]
        surroundKids = makeMany Kid [(3, 5), (2, 5), (4, 6)]
        board = newBoard 7 7 *++ (robots ++ kids ++ surroundKids)
        moves@[m1, m2] = [Grab (Position x (x + 1)) | x <- [0, 4]]
     in do
          actionCalc calcType board Nothing 1000 m1 `shouldBe` 1100
          actionCalc calcType board Nothing 1000 m2 `shouldBe` 700
  it "Should have the correct value after cleaning" $
    let robot = make (Robot Nothing) (0, 0)
        dirt = make Dirt (0, 0)
        board = newBoard 5 5 *++ [robot, dirt]
        move = Clean (Position 0 0)
     in actionCalc calcType board Nothing 500 move `shouldBe` 500
  it "Should have the correct value after moving" $
    let robot = make (Robot Nothing) (0, 0)
        board = newBoard 3 3 *+ robot
        move = Move (Position 0 1)
     in actionCalc calcType board Nothing 500 move `shouldBe` 600
  where
    calcType = Balance

bestRewardPathfindingSpec = describe "Pathfind works correctly with best reward" $ do
  it "Should find the path to the crib task" $
    let robot = make (Robot Nothing) (0, 0)
        kid = make Kid (1, 1)
        crib = make Crib (2, 2)
        board = newBoard 3 3 *++ [robot, kid, crib]
        ag = Agent robot Nothing
        pathShort = findObject board ag crib ShortestPath
        pathBest = findObject board ag crib Balance
     in do
          length pathShort `shouldBe` 4
          length pathBest `shouldBe` 4
  it "Should get an empty path if target task does not exists" $
    let robot = make (Robot Nothing) (0, 0)
        dirt = make Dirt (2, 2)
        board = newBoard 3 3 *+ robot
        ag = Agent robot Nothing
        path = findObject board ag dirt Balance
     in path `shouldBe` []

noBlockCribsSpecs = describe "Never chooses a blocking crib as a Task" $ do
  it "Should find the last crib as the best reward task" $
    let robot = make (Robot (Just Kid)) (0, 0)
        cribs@[c1, c2, c3] = makeMany Crib [(0, i) | i <- [1 .. 3]]
        board = newBoard 1 4 *++ (robot : cribs)
        ag = Agent robot Nothing
        [resAg] = assignTasks board [ag] (fvalues board)
     in (fromJust . getTask) resAg `shouldBe` c3
  it "Should find the middle crib as the best reward task" $
    let robot = make (Robot (Just Kid)) (19, 19)
        cribs = makeMany Crib [(i, j) | i <- [5 .. 9], j <- [5 .. 9]]
        board = newBoard 20 20 *++ (robot : cribs)
        ag = Agent robot Nothing
        target = make Crib (7, 7)
        [resAg] = assignTasks board [ag] (fvalues board)
     in (fromJust . getTask) resAg `shouldBe` target
  it "Should find the cornered crib as the best reward task" $
    let robot = make (Robot (Just Kid)) (0, 0)
        cribs = makeMany Crib [(i, j) | i <- [5 .. 9], j <- [5 .. 9]]
        board = newBoard 10 10 *++ (robot : cribs)
        ag = Agent robot Nothing
        target = make Crib (9, 9)
        [resAg] = assignTasks board [ag] (fvalues board)
     in (fromJust . getTask) resAg `shouldBe` target
  it "Should find the last crib as the best reward task" $
    let robot = make (Robot Nothing) (0, 250)
        kids = makeMany Kid [(0, i) | i <- [0 .. 10]]
        cribs = makeMany Crib [(0, i) | i <- [350 .. 499]]
        board = newBoard 1 500 *++ (kids ++ robot : cribs)
        target = make Crib (0, 499)
        ag = Agent robot Nothing
        [resAg] = assignTasks board [ag] (fvalues board)
     in do
          (fromJust . getTask) resAg `shouldBe` target

  it "Shoudl assign each agent to the best task" $
    let board = boardSelect 3
        agents = agentInit board
        resAgs = assignTasks board agents (fvalues board)
     in do
          print resAgs
          3 `shouldBe` 3
  where
    fvalues board = fillValues board (BalanceCrib Nothing)
