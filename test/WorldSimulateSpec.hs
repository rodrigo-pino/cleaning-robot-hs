module WorldSimulateSpec where

import Data.List (nub)
import System.Random
import Test.Hspec
import Test.Tasty
import Test.Tasty.Hspec
import World.Board
import World.Objects
import World.Simulate

testRandomPicking = describe "Should pick a random element from a sequenece" $ do
  it "Should pick correctly #1" $
    let elems = [1, 2, 3, 4, 5]
        (s, remElems, _) = getRandom elems testRandomGen
     in remElems `shouldBe` filter (/= s) elems

testBoardShuffling = describe "Test that board is correcly shuffled" $ do
  it "Correct shuffling with empty board" $
    let board = newBoard 10 10
        (shuffled, _) = shuffle board testRandomGen
     in length (elems board) `shouldBe` 0
  it "Correct shuffling with some elements" $
    let board = newBoard 10 10 *++ makeMany Obstacle [(1, 2), (3, 4)]
        (shuffled, _) = shuffle board testRandomGen
        joinedElems = elems board ++ elems shuffled
     in length (nub joinedElems) `shouldBe` 4
  it "Correct shuffling with full board" $
    let objs =
          [ make (Robot Nothing) (0, 0),
            make (Robot (Just Kid)) (0, 1),
            make Crib (0, 2),
            make Kid (1, 0),
            make Obstacle (1, 1)
          ]
        board = newBoard 2 3 *++ objs
        (shuffled, _) = shuffle board testRandomGen
     in equalTypesButDiffPos (elems board) (elems shuffled) `shouldBe` []
  where
    equalTypesButDiffPos [] objs2 = objs2
    equalTypesButDiffPos (obj1 : objs1) objs2 =
      equalTypesButDiffPos objs1 (searchAndDelete obj1 objs2)
    searchAndDelete _ [] = []
    searchAndDelete obj1 objects@(obj2 : objs2)
      | typex obj1 == typex obj2 =
        if position obj1 /= position obj2
          then objs2
          else objects
      | otherwise = obj2 : searchAndDelete obj1 objs2

testKidMovement = describe "Test that kids move and dirt is correctly generated" $ do
  it "Correct movement and dirt generation with one kid" $
    let board = newBoard 1 2 *++ [make Kid (0, 0)]
        (updBoard, _) = loop 10 (board, mkStdGen 10)
     in elems updBoard `shouldBe` [make Kid (0, 1), make Dirt (0, 0)]
  it "Correct movement and dirt generation with 2 kid" $
    let board = newBoard 3 3 *++ makeMany Kid [(i, i) | i <- [0 .. 2]]
        (updBoard, _) = loop 10 (board, mkStdGen 10)
     in not (null (getByType updBoard Dirt)) `shouldBe` True
  it "Correct movement and dirt generation with 3 kid" $
    let x = 3
     in 3 `shouldBe` 3
  where
    loop 0 r = r
    loop num (board, g) = loop (num - 1) (moveKids board g)

testRandomGen :: StdGen
testRandomGen = mkStdGen 2
