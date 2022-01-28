module Visual where

import Data.List (sort)
import Data.Maybe (fromJust)
import GHC.Base (join)
import World.Objects

printState board =
  printBoard (drawBoard board 0)

printBoard :: [[Char]] -> IO ()
printBoard [] = return ()
printBoard (row : rows) = do
  print row
  printBoard rows

drawBoard :: Board -> Int -> [[Char]]
drawBoard board i
  | i == maxR = [delimiter]
  | otherwise = [delimiter, drawRow (getRow board i)] ++ drawBoard board (i + 1)
  where
    maxR = maxRows board
    delimiter = drawDelimiter (maxCols board - 1)

drawRow :: [[ObjectType]] -> [Char]
drawRow [] = "|"
drawRow (typex : types)
  | null typex = "|   " ++ drawRow types
  | otherwise = "| " ++ [drawChar typex, ' '] ++ drawRow types

drawDelimiter :: Int -> [Char]
drawDelimiter amount = concat ["+ - " | _ <- [0 .. amount]] ++ ['+']

drawChar :: [ObjectType] -> Char
drawChar objTypes =
  case objTypes of
    [Crib] -> 'c'
    [Crib, Kid] -> 'C'
    [Crib, Robot Nothing] -> 'g'
    [Crib, Kid, Robot Nothing] -> 'G'
    [Crib, Robot (Just Kid)] -> 'G'
    [Dirt] -> 'd'
    [Dirt, Robot Nothing] -> 'D'
    [Dirt, Robot (Just Kid)] -> 'D'
    [Kid] -> 'k'
    [Kid, Robot Nothing] -> 'K'
    [Obstacle] -> 'o'
    [Robot Nothing] -> 'r'
    [Robot (Just Kid)] -> 'R'
    _ -> error ("Invalid combination: " ++ show objTypes)
