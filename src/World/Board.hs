module World.Board where

import Data.Maybe (fromJust, isJust, isNothing)
import World.Objects

moves :: Object -> Board -> [Maybe (Action Position)]
moves obj board
  | carries = map (move obj board) directions2
  | isKid || isRobot = map (move obj board) directions
  | otherwise = error "Movement is not defined for this object"
  where
    objType = typex obj
    carries = objType == Robot (Just Kid)
    isRobot = objType == Robot Nothing
    isKid = objType == Kid

move :: Object -> Board -> Position -> Maybe (Action Position)
move (Object Kid pos) board dir
  | isNothing cell || cannotMove = Nothing
  | null objTypes = Just (Move goto)
  | objTypes == [Obstacle] =
    case dir of
      Position i 0 ->
        let (case1, len1) = canPush (getRow board (row pos)) (col pos) i
         in if case1
              then Just (Push goto (Position i len1))
              else Nothing
      Position 0 j ->
        let (case2, len2) = canPush (getCol board (col pos)) (row pos) j
         in if case2
              then Just (Push goto (Position len2 j))
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
        && (fromJust (board ! pos) == [Robot (Just Kid)])

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
  | null x = (True, acc)
  | x == [Obstacle] = movableObstacle xs (acc + 1)
  | otherwise = (False, acc)
