{-# OPTIONS_GHC -Wno-missing-methods #-}

module World.Objects where

import Data.Hashable
import Data.List (foldl', nub, sort)
import Data.Maybe
import Debug.Trace (trace)

data Position = Position {row :: Int, col :: Int}

data ObjectType
  = Crib
  | Dirt
  | Kid
  | Obstacle
  | Robot {carry :: Maybe ObjectType}
  deriving (Eq, Ord, Show)

data Object = Object {typex :: ObjectType, position :: Position}

data Board = Board {elems :: [Object], maxRows :: Int, maxCols :: Int}

data Action a = Clean a | Drop a | Grab a | Move a | Push a a deriving (Eq, Show)

instance Hashable ObjectType where
  hashWithSalt salt objType = (hash . show) objType

instance (Ord a) => Ord (Action a) where
  a1 <= a2 = value a1 <= value a2

instance (Hashable a) => Hashable (Action a) where
  hashWithSalt salt action = (hash . value) action

instance Num Position where
  (Position x1 y1) + (Position x2 y2) = Position (x1 + x2) (y1 + y2)

instance Show Position where
  show (Position rowx coly) = "(" ++ show rowx ++ "," ++ show coly ++ ")"

instance Eq Position where
  (Position x1 y1) == (Position x2 y2) = x1 == x2 && y1 == y2

instance Ord Position where
  (Position x1 y1) <= (Position x2 y2) = x1 <= x2 && y1 <= y2

instance Hashable Position where
  hashWithSalt salt (Position a b) = hash (a, b)

instance Eq Object where
  obj1 == obj2 = typex obj1 == typex obj2 && position obj1 == position obj2

instance Ord Object where
  obj1 <= obj2 = position obj1 <= position obj2 && typex obj1 <= typex obj2

instance Show Object where
  show obj = "{" ++ show (typex obj) ++ ":" ++ show (position obj) ++ "}"

instance Hashable Object where
  hashWithSalt salt obj = hash (show obj)

class Movable a where
  update :: a -> Position -> a

instance Movable Object where
  update obj pos = Object {typex = typex obj, position = pos}

instance Eq Board where
  b1 == b2 =
    maxRows b1 == maxRows b2
      && maxCols b1 == maxCols b2
      && (sort (elems b1) == sort (elems b2))

instance Show Board where
  show b = show (elems b)

class IBoard a where
  (*++) :: a -> [Object] -> a
  (*--) :: a -> [Object] -> a
  (*+) :: a -> Object -> a
  (*-) :: a -> Object -> a
  (!) :: a -> Position -> Maybe [ObjectType]
  getRow :: a -> Int -> [[ObjectType]]
  getCol :: a -> Int -> [[ObjectType]]

instance IBoard Board where
  board *++ objs = Board (elems board ++ objs) (maxRows board) (maxCols board)
  board *-- objs =
    let newCells = filter (`notElem` objs) (elems board)
     in if length (elems board) /= length newCells + length objs
          then error ("Element to delete does not exists: " ++ show objs)
          else Board newCells (maxRows board) (maxCols board)
  board *- obj = board *-- [obj]
  board *+ obj = board *++ [obj]
  board ! pos =
    let objTypes = [typex obj | obj <- elems board, pos == position obj]
        valid =
          let r = row pos
              c = col pos
           in r < maxRows board
                && c < maxCols board
                && r >= 0
                && c >= 0
     in if valid
          then Just (sort objTypes)
          else Nothing
  getRow board i =
    [ fromJust val
      | j <- [0 .. maxCols board - 1],
        let val = board ! Position i j
    ]
  getCol board j =
    [ fromJust val
      | i <- [0 .. maxRows board - 1],
        let val = board ! Position i j
    ]

newBoard :: Int -> Int -> Board
newBoard = Board []

popTypes :: Board -> [ObjectType] -> (Board, [Object])
popTypes board objTypes = (Board keep n m, remove)
  where
    m = maxCols board
    n = maxCols board
    (remove, keep) = foldl' popFunc ([], []) (elems board)
    popFunc (remove, keep) val =
      let updRemove = val : remove
          updKeep = val : keep
       in if typex val `elem` objTypes
            then updRemove `seq` (updRemove, keep)
            else updKeep `seq` (remove, updKeep)

-- Returns all adyacent ObjecTypes of a certain position
adyacentsTo :: Board -> Position -> [[ObjectType]]
adyacentsTo board pos = catMaybes [board ! (pos + dir) | dir <- directions1]

-- Returns empty positios around a position. If the position is empty it is returned as well
emptyAround :: Board -> Position -> [Position]
emptyAround board pos =
  [ pos + dir
    | dir <- p0 : directions1,
      let cell = board ! (pos + dir),
      isJust cell && null (fromJust cell)
  ]
  where
    p0 = Position 0 0

getByType :: Board -> ObjectType -> [Object]
getByType board objType = [obj | obj <- elems board, typex obj == objType]

getByTypes :: Board -> [ObjectType] -> [Object]
getByTypes board objTypes = [obj | obj <- elems board, typex obj `elem` objTypes]

make :: ObjectType -> (Int, Int) -> Object
make objType pos = Object objType (positionFromTuple pos)

makeMany :: ObjectType -> [(Int, Int)] -> [Object]
makeMany objType = map (make objType)

positionFromTuple :: (Int, Int) -> Position
positionFromTuple = uncurry Position

tupleFromPosition :: Position -> (Int, Int)
tupleFromPosition (Position x y) = (x, y)

directions :: [Position]
directions = map positionFromTuple [(1, 0), (-1, 0), (0, 1), (0, -1)]

directions1 :: [Position]
directions1 = directions ++ map positionFromTuple [(1, 1), (-1, 1), (1, -1), (-1, -1)]

directions2 :: [Position]
directions2 = directions ++ nub [d1 + d2 | d1 <- directions, d2 <- directions]

value :: Action a -> a
value (Clean v) = v
value (Drop v) = v
value (Grab v) = v
value (Move v) = v
value (Push v _) = v
