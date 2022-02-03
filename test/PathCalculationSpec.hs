module PathCalculationSpec where

import Agent.Logic.Pathfinding.Find
import Agent.Logic.Pathfinding.PathCalculation
import Agent.Objects
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
    calcType = BestReward

bestRewardPathfindingSpec = describe "Pathfind works correctly with best reward" $ do
  it "Should find the path to the crib task" $
    let robot = make (Robot Nothing) (0, 0)
        kid = make Kid (1, 1)
        crib = make Crib (2, 2)
        board = newBoard 3 3 *++ [robot, kid, crib]
        ag = Agent robot Nothing
        pathShort = findObject board ag crib ShortestPath
        pathBest = findObject board ag crib BestReward
     in do
          length pathShort `shouldBe` 4
          length pathBest `shouldBe` 4
  it "Should get an empty path if target task does not exists" $
    let robot = make (Robot Nothing) (0, 0)
        dirt = make Dirt (2, 2)
        board = newBoard 3 3 *+ robot
        ag = Agent robot Nothing
        path = findObject board ag dirt BestReward
     in path `shouldBe` []
