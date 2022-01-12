module WorldSpec where

import Test.Hspec
import Test.Tasty
import Test.Tasty.Hspec
import World.Board
import World.Objects

boardPrimitiveTest :: SpecWith ()
boardPrimitiveTest = describe "Basic board function test" $ do
  it "Should return a column" $
    getCol (board *++ makeMany Obstacle [(i, 1) | i <- [0 .. rows - 1]]) 1
      `shouldBe` [[Obstacle] | _ <- [1 .. rows]]
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
     in moves kid (board *++ (kid : obstacles))
          `shouldBe` [ Move (pos (2, 1)),
                       Move (pos (0, 1)),
                       Push (pos (1, 2)) (pos (1, 5)),
                       Move (pos (1, 0))
                     ]
  it "Should push in every direction" $
    let kid = make Kid (2, 2)
        obstacles = makeMany Obstacle [(2, 1), (1, 2), (3, 2), (2, 3), (2, 4)]
     in moves kid (board *++ (kid : obstacles))
          `shouldBe` [ Push (pos (3, 2)) (pos (4, 2)),
                       Push (pos (1, 2)) (pos (0, 2)),
                       Push (pos (2, 3)) (pos (2, 5)),
                       Push (pos (2, 1)) (pos (2, 0))
                     ]
  it "Should not be able to move" $
    let kid = make Kid (2, 2)
        obstacles =
          makeMany
            Obstacle
            [ (2, 0),
              (2, 1),
              (0, 2),
              (1, 2),
              (3, 2),
              (4, 2),
              (2, 3),
              (2, 4),
              (2, 5)
            ]
     in moves kid (board *++ (kid : obstacles)) `shouldBe` []
  where
    board = newBoard 5 6

robotWithoutKidTest :: SpecWith ()
robotWithoutKidTest = describe "Robot movement without kid" $ do
  it "Should clean or move to dirty cells" $
    let robot = make rob (2, 2)
        dirtCells = makeMany Dirt [(2, 1), (1, 2), (2, 2), (3, 2), (2, 3)]
     in moves robot (board *++ (robot : dirtCells))
          `shouldBe` [ Move (pos (3, 2)),
                       Move (pos (1, 2)),
                       Move (pos (2, 3)),
                       Move (pos (2, 1)),
                       Clean (pos (2, 2))
                     ]
  it "Should grab kids or move to cells with cribs" $
    let robot = make rob (2, 2)
        kids = makeMany Kid [(2, 1), (1, 2)]
        cribs = makeMany Crib [(3, 2), (2, 3)]
     in moves robot (board *++ (robot : kids))
          `shouldBe` [ Move (pos (3, 2)),
                       Grab (pos (1, 2)),
                       Move (pos (2, 3)),
                       Grab (pos (2, 1))
                     ]
  it "Should respect boundries" $
    let robot1 = make rob (0, 0)
        robot2 = make rob (4, 5)
     in do
          moves robot1 (board *+ robot1) `shouldBe` [Move (pos (1, 0)), Move (pos (0, 1))]
          moves robot2 (board *+ robot2) `shouldBe` [Move (pos (3, 5)), Move (pos (4, 4))]
  it "Should not be able to move when blocked by obstacles" $
    let robot = make (Robot Nothing) (0, 0)
        obstacles = makeMany Obstacle [(0, 1), (1, 0)]
     in moves robot (board *++ (robot : obstacles)) `shouldBe` []
  where
    rob = Robot Nothing
    board = newBoard 5 6

robotWithKidTest = describe "Robot movement when carrying a kid" $ do
  it "Should have greater moving range" $
    let robot = make rob (4, 4)
     in moves robot (board *+ robot)
          `shouldBe` [ Move (pos (5, 4)),
                       Move (pos (3, 4)),
                       Move (pos (4, 5)),
                       Move (pos (4, 3)), -- until here the first directions
                       Move (pos (6, 4)),
                       Move (pos (4, 4)),
                       Move (pos (5, 5)),
                       Move (pos (5, 3)),
                       Move (pos (2, 4)),
                       Move (pos (3, 5)),
                       Move (pos (3, 3)),
                       Move (pos (4, 6)),
                       Move (pos (4, 2))
                     ]
  it "Should not be able to move when surrounded by kids" $
    let robot = make rob (4, 4)
        kids = makeMany Kid [(3, 4), (5, 4), (4, 3), (4, 5)]
     in moves robot (board *++ (robot : kids)) `shouldBe` []
  where
    board = newBoard 10 10
    rob = Robot (Just Kid)

pos = positionFromTuple
