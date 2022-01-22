module World.Simulate where

import Data.Map (Map (..))
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Debug.Trace (trace)
import System.Random
import World.Board
import World.Objects hiding ((!))

type KidCluster = Map Object Int

worldInit :: Int -> Int -> [Object] -> Board
worldInit rows cols objects = Board objects rows cols

worldSim :: Board -> StdGen -> Bool -> (Board, StdGen)
worldSim board g True = shuffle board g
worldSim board g False = moveKids board g

moveKids :: Board -> StdGen -> (Board, StdGen)
moveKids board g =
  let kidCluster = getKidCluster board
   in Map.foldlWithKey toMoveOrNotMove (board, g) kidCluster
  where
    -- (acc -> key -> val) -> acc
    toMoveOrNotMove :: (Board, StdGen) -> Object -> Int -> (Board, StdGen)
    toMoveOrNotMove (board, g) kid kidsNear
      | nextStep >= length posMoves = (board, newG)
      | otherwise =
        let updatedBoard = applyMove kid nextMove board
            emptyNear = emptyAround updatedBoard (position kid)
            (dirtAmount, newNewG) = case kidsNear of
              0 -> randomR (0, 1) newG
              1 -> randomR (0, 3) newG
              _ -> randomR (0, 6) newG
         in toDirt updatedBoard emptyNear dirtAmount newNewG
      where
        posMoves = moves kid board
        nextMove = head posMoves
        (nextStep, newG) = randomR (0, 4) g

    toDirt :: Board -> [Position] -> Int -> StdGen -> (Board, StdGen)
    toDirt board positions amount g
      | null positions || amount == 0 = (board, g)
      | otherwise =
        let (pos, remPositions, newG) = getRandom positions g
         in toDirt (board *+ Object Dirt pos) remPositions (amount - 1) newG

shuffle :: Board -> StdGen -> (Board, StdGen)
shuffle board g = (Board newElems n m, newG)
  where
    (newElems, newG) = relocateObjects boardElems allPos g
    allPos = [Position i j | i <- [0 .. n - 1], j <- [0 .. m - 1]]
    boardElems = elems board
    n = maxRows board
    m = maxCols board

relocateObjects :: [Object] -> [Position] -> StdGen -> ([Object], StdGen)
relocateObjects [] _ g = ([], g)
relocateObjects (obj : objects) positions g = (newObj : newObjs, lastG)
  where
    (newPos, remPositions, newG) = getRandom positions g
    newObj = Object (typex obj) newPos
    (newObjs, lastG) = relocateObjects objects remPositions newG

getRandom :: [a] -> StdGen -> (a, [a], StdGen)
getRandom [] _ = error "Out of positions"
getRandom positions g = (selectedPos, reverse remPositions, newG)
  where
    nextP :: Int
    (nextP, newG) = randomR (0, length positions - 1) g

    (Just selectedPos, _, remPositions) = foldl extract (Nothing, 0, []) positions
    extract (ext, i, list) val
      | i == nextP = (Just val, i + 1, list)
      | otherwise = (ext, i + 1, val : list)
