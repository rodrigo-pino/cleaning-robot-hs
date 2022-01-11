module WorldSpec where

import Test.Hspec
import Test.Tasty
import Test.Tasty.Hspec
import World.Board
import World.Objects

boardPrimitiveTest :: SpecWith ()
boardPrimitiveTest = describe "Basic board function test" $ do
  it "Should return an empty board row" $
    getRow board 1 `shouldBe` [[] | _ <- [1 .. rows]]
  it "Obstacle should be movable" $
    movableObstacle pushable 0 `shouldBe` (True, 4)
  it "Obstacle should not be movable" $
    movableObstacle obstacles 0 `shouldBe` (False, 3)
  it "Obstacle can be pushed forward" $
    canPush wholeRow 4 1 `shouldBe` (True, 4)
  it "Obstacle can be pushed backward" $
    canPush wholeRow 4 (-1) `shouldBe` (True, 4)
  where
    rows = 4
    cols = 5
    board = newBoard rows cols
    obstacles = [[Obstacle] | _ <- [1 .. 3]]
    pushable = obstacles ++ [[]]
    wholeRow = reverse pushable ++ [[Kid]] ++ pushable

kidTest :: SpecWith ()
kidTest = describe "Kid movement" $ do
  it "Should move or push obstacles" $
    let kid = make Kid (1, 1)
        obstacles = makeMany Obstacle [(1, 2), (1, 3), (1, 4)]
        board = newBoard 5 6 *++ (kid : obstacles)
     in moves kid board
          `shouldBe` [ Move (pos (2, 1)),
                       Move (pos (0, 1)),
                       Push (pos (1, 2)) (pos (1, 5)),
                       Move (pos (1, 0))
                     ]

pos = positionFromTuple
