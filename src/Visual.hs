module Visual where

import Data.List (foldl', nub, sort)
import Data.Maybe (fromJust)
import GHC.Base (join)
import World.Objects

printState board = do
  printBoard (drawBoard board 0)
  print (drawLegend board)

printBoard :: [[Char]] -> IO ()
printBoard [] = return ()
printBoard (row : rows) = do
  print row
  printBoard rows

drawLegend :: Board -> String
drawLegend board
  | not (null objTypes) = word ++ foldl' (\acc val -> acc ++ ", " ++ val) "" words
  | otherwise = ""
  where
    objTypes = filter (not . null) ((sort . nub . concat) [getRow board i | i <- [0 .. maxRows board - 1]])
    (word : words) = [drawChar ots : '-' : '>' : drawTypes ots | ots <- objTypes]

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

-- drawTypes :: [ObjectType] -> String
drawTypes objTypes =
  case objTypes of
    [Crib] -> "Crib"
    [Crib, Kid] -> "Crib with Kid"
    [Crib, Robot Nothing] -> "Crib and Robot"
    [Crib, Kid, Robot Nothing] -> "Crib with Kid and Robot"
    [Crib, Robot (Just Kid)] -> "Crib and Robot with Kid"
    [Dirt] -> "Dirt"
    [Dirt, Robot Nothing] -> "Dirt and Robot"
    [Dirt, Robot (Just Kid)] -> "Dirt and Robot with Kid"
    [Kid] -> "Kid"
    [Kid, Robot Nothing] -> "Kid and Robot"
    [Obstacle] -> "Obstacle"
    [Robot Nothing] -> "Robot"
    [Robot (Just Kid)] -> "Robot with Kid"
    _ -> error ("Invalid combination: " ++ show objTypes)
