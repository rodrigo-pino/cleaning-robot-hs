module TaskHandlingSpec where

import Agent.Logic.TaskHandling
import Agent.Objects (Task (..))
import Test.Hspec
import Test.Tasty
import Test.Tasty.Hspec
import World.Board
import World.Objects

findTaskTest = describe "Detect all tasks on board" $ do
  it "Should detect a cleaning task" $
    let dirt = make Dirt (5, 5)
     in getTasks (board *+ dirt) `shouldBe` [Task {target = dirt, solvers = []}]
  it "Should detect a moving kid to crib task" $
    let crib = make Crib (5, 5)
     in getTasks (board *+ crib) `shouldBe` [Task {target = crib, solvers = []}]
  where
    board = newBoard 10 10

-- optimizeTest = describe "Optimal distribution of tasks" $ do
-- it ""
