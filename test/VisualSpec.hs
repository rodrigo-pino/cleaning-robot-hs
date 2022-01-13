module VisualSpec where

import Data.List (sort)
import Test.Hspec
import Test.Tasty
import Test.Tasty.Hspec
import Visual
import World.Board
import World.Objects

visualTest :: SpecWith ()
visualTest = describe "Visualizing board state" $ do
  it "Should print all objects correctly" $
    let board =
          newBoard 3 2
            *++ [ make Crib (0, 0),
                  make Dirt (1, 0),
                  make Kid (2, 0),
                  make Obstacle (0, 1),
                  make (Robot Nothing) (1, 1),
                  make (Robot (Just Kid)) (2, 1)
                ]
        delimiter = drawDelimiter 1
        repr =
          [ delimiter,
            "| c | o |",
            delimiter,
            "| d | r |",
            delimiter,
            "| k | R |",
            delimiter
          ]
     in representation board `shouldBe` repr
  it "Should print all combinations correctly" $
    let board =
          newBoard 2 4
            *++ makeMany Crib [(0, 0), (0, 1), (0, 2), (0, 3)]
            *++ makeMany Kid [(0, 0), (0, 2), (1, 1)]
            *++ makeMany (Robot Nothing) [(0, 1), (0, 2), (1, 0), (1, 1)]
            *++ makeMany (Robot (Just Kid)) [(0, 3), (1, 2)]
            *+ make Dirt (1, 0)
        delimiter = drawDelimiter 3
        repr =
          [ delimiter,
            "| C | g | G | G |",
            delimiter,
            "| D | K | R |   |",
            delimiter
          ]
     in representation board `shouldBe` repr
  it "Should print white spaces" $
    let board = newBoard 5 4 *+ make Kid (2, 3)
        delimiter = drawDelimiter 3
        emptyLine = concat ["|   " | _ <- [1 .. 4]] ++ "|"
        repr =
          [ delimiter,
            emptyLine,
            delimiter,
            emptyLine,
            delimiter,
            "|   |   |   | k |",
            delimiter,
            emptyLine,
            delimiter,
            emptyLine,
            delimiter
          ]
     in representation board `shouldBe` repr

representation :: Board -> [[Char]]
representation board = drawBoard board 0
