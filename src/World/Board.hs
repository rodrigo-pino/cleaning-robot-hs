module World.Board where

import Data.Maybe (catMaybes, fromJust, isJust, isNothing)
import World.Objects

moves :: Object -> Board -> [Action Position]
moves obj board =
  let allMoves = case typex obj of
        Robot (Just Kid) -> map (move obj board) directions2
        Robot Nothing -> map (move obj board) directions
        Kid -> map (move obj board) directions
        _ -> error "Movement is not defined for this object"
   in catMaybes allMoves

move :: Object -> Board -> Position -> Maybe (Action Position)
move (Object Kid pos) board dir
  | isNothing cell || cannotMove = Nothing
  | null objTypes = Just (Move goto)
  | objTypes == [Obstacle] =
    let (rowPos, colPos) = tupleFromPosition pos
     in case dir of
          Position i 0 ->
            let (case1, len1) = canPush (getCol board colPos) rowPos i
             in if case1
                  then Just (Push goto (Position (rowPos + i * len1) colPos))
                  else Nothing
          Position 0 j ->
            let (case2, len2) = canPush (getRow board rowPos) colPos j
             in if case2
                  then Just (Push goto (Position rowPos (colPos + j * len2)))
                  else Nothing
  | otherwise = Nothing
  where
    goto = pos + dir
    cell = board ! (pos + dir)
    objTypes = fromJust cell
    cannotMove = not (null [t | t <- objTypes, t == Crib])
move (Object (Robot carries) pos) board dir
  | canClean = Just (Clean pos)
  | canDrop = Just (Drop pos)
  | isNothing cell = Nothing
  | objTypes == [Kid] = Just (Grab pos)
  | objTypes `elem` [[Crib], [Dirt], []] = Just (Move pos)
  | otherwise = Nothing
  where
    goto = pos + dir
    cell = board ! (pos + dir)
    objTypes = fromJust cell
    canClean =
      (dir == Position 0 0)
        && (fromJust (board ! pos) == [Dirt, Robot Nothing])
    canDrop =
      (dir == Position 0 0)
        && (fromJust (board ! pos) == [Crib, Robot (Just Kid)])

applyMove :: Object -> Action Position -> Board -> Board
applyMove obj (Clean pos) board = board *- Object Dirt pos
applyMove obj (Drop pos) board = board *+ Object Kid pos
applyMove obj (Grab pos) board =
  (board *-- [Object Kid pos, obj]) *+ Object (Robot (Just Kid)) pos
applyMove obj (Move pos) board = (board *- obj) *+ update obj pos
applyMove obj (Push pos1 pos2) board =
  (board *- Object Obstacle pos1) *++ [update obj pos1, Object Obstacle pos2]

canPush :: [[ObjectType]] -> Int -> Int -> (Bool, Int)
canPush xs posi diri
  | diri == 1 = movableObstacle (drop (posi + 1) xs) 0
  | diri == -1 = movableObstacle (reverse (take posi xs)) 0

movableObstacle :: [[ObjectType]] -> Int -> (Bool, Int)
movableObstacle [] acc = (False, acc)
movableObstacle (x : xs) acc
  | null x = (True, acc + 1)
  | x == [Obstacle] = movableObstacle xs (acc + 1)
  | otherwise = (False, acc)
