module World.Simulate where

import System.Random
import World.Objects

worldInit :: Int -> Int -> [Object] -> Board
worldInit rows cols objects = Board objects rows cols

worldSim :: Board -> Board
worldSim board = board

moveKids :: Board -> Board
moveKids board = board

generateDirt :: Board -> Board
generateDirt board = board

shuffle :: Board -> StdGen -> (Board, StdGen)
shuffle board g = (Board newElems n m, newG)
  where
    (newElems, newG) = relocateObjects boardElems allPos g
    allPos = [(i, j) | i <- [0 .. n - 1], j <- [0 .. m - 1]]
    boardElems = elems board
    n = maxRows board
    m = maxCols board

relocateObjects :: [Object] -> [(Int, Int)] -> StdGen -> ([Object], StdGen)
relocateObjects [] _ g = ([], g)
relocateObjects (obj : objects) positions g = (newObj : newObjs, lastG)
  where
    (newPos, remPositions, newG) = getPosition positions g
    newObj = make (typex obj) newPos
    (newObjs, lastG) = relocateObjects objects remPositions newG

getPosition :: (Num a, Num b) => [(a, b)] -> StdGen -> ((a, b), [(a, b)], StdGen)
getPosition [] _ = error "Out of positions"
getPosition positions g = (selectedPos, remPositions, newG)
  where
    nextP :: Int
    (nextP, newG) = randomR (0, length positions) g

    (selectedPos, _, remPositions) = foldl extract ((-1, -1), 0, []) positions
    extract (ext, i, list) val
      | i == nextP = (val, i + 1, list)
      | otherwise = (ext, i + 1, val : list)
