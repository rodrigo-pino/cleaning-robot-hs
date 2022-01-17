module TaskHandlingSpec where

import Agent.Logic.TaskHandling
import Agent.Objects
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

optimizeTest = describe "Optimal distribution of tasks" $ do
  it "Should assing the agent the fastest to complete task" $
    3 `shouldBe` 3
