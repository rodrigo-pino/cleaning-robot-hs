module World.Board where

import Data.List (delete, nub)
import Data.Map (Map (..))
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromJust, isJust, isNothing)
import GHC.List (foldl')
import World.Objects

moves :: Object -> Board -> [Action Position]
moves obj board =
  let allMoves = case typex obj of
        Robot (Just Kid) ->
          let fstSteps = map (move obj board) directions0
              sndSteps =
                [ posMove
                  | newObj <- map (update obj . value) (catMaybes fstSteps),
                    posMove <- map (move newObj board) directions
                ]
           in nub (fstSteps ++ sndSteps)
        Robot Nothing -> map (move obj board) directions0
        Kid -> map (move obj board) directions
        _ -> error ("Movement is not defined for object: " ++ show obj)
   in catMaybes allMoves
  where
    directions0 = directions ++ [Position 0 0]
    dirt (Just action) = value action

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
    cannotMove = Crib `elem` fromJust (board ! pos)
move (Object (Robot carries) pos) board dir
  | canClean = Just (Clean goto)
  | canDrop = Just (Drop goto)
  | isNothing cell = Nothing
  | canGrab = Just (Grab goto)
  | objTypes `elem` [[Crib], [Dirt], []] = Just (Move goto)
  | otherwise = Nothing
  where
    goto = pos + dir
    cell = board ! (pos + dir)
    objTypes = fromJust cell
    canClean =
      dir == Position 0 0
        && fromJust (board ! pos) == [Dirt, Robot Nothing]
    canDrop =
      dir == Position 0 0
        && fromJust (board ! pos) `elem` [[Robot (Just Kid)], [Crib, Robot (Just Kid)]]
    canGrab =
      objTypes == [Kid]
        && dir /= Position 0 0
        && isNothing carries

applyMove :: Object -> Action Position -> Board -> Board
applyMove _ (Clean pos) board = board *- Object Dirt pos
applyMove obj (Drop pos) board = (board *- obj) *++ [Object Kid pos, Object (Robot Nothing) pos]
applyMove obj (Grab pos) board =
  (board *-- [Object Kid pos, obj]) *+ Object (Robot (Just Kid)) pos
applyMove obj (Move pos) board = (board *- obj) *+ update obj pos
applyMove obj (Push pos1 pos2) board =
  (board *-- [Object Obstacle pos1, obj]) *++ [update obj pos1, Object Obstacle pos2]

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

getKidCluster :: Board -> Map Object Int
getKidCluster board = clusters
  where
    kids = getByType board Kid
    clusters = foldl' addCluster Map.empty kids
    addCluster acc kid =
      let kidsAround = length (filter ([Kid] ==) (adyacentsTo board (position kid)))
       in Map.insert kid kidsAround acc

calculateDirtiness :: Board -> Float
calculateDirtiness board =
  let totalDirts = length (getByType board Dirt)
      totalBlocks = maxRows board * maxCols board
   in (fromIntegral totalDirts / fromIntegral totalBlocks) * 100

allKidsInCribs board =
  let kids = getByType board Kid
   in and
        [ r
          | kid <- kids,
            let pos = position kid,
            let r = Crib `elem` fromJust (board ! pos)
        ]
