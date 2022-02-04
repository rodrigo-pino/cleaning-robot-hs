module Agent.Logic.Pathfinding.PathCalculation where

import Agent.Objects
import Control.Parallel (par, pseq)
import Data.HashMap.Strict (HashMap (..))
import qualified Data.HashMap.Strict as Map
import Data.Maybe (fromJust, isJust, isNothing)
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Set (Set (..))
import qualified Data.Set as Set
import Debug.Trace (trace)
import GHC.List (foldl')
import World.Objects

data PathCalcType
  = ShortestPath
  | Balance
  | BalanceCrib {values :: Maybe (HashMap Position Natural)}

actionCalc :: PathCalcType -> Board -> Maybe Object -> Natural -> Action Position -> Natural
actionCalc typex board target acc move =
  case typex of
    ShortestPath -> acc + 1
    Balance -> balanceCalc board acc move target
    BalanceCrib maybeMap -> balanceCribCalc board acc move target (fromJust maybeMap)

balanceCalc :: Board -> Natural -> Action Position -> Maybe Object -> Natural
balanceCalc board acc@(Natural val) move target =
  case move of
    Drop pos
      | fromJust (board ! pos) == [Crib, Robot (Just Kid)] ->
        min
          (Natural (val `div` 2)) -- to reward long paths
          (acc - baseMoveCost * avg) -- to reward short paths
      | otherwise -> acc + baseMoveCost * (1 + 2 * kidsAround pos)
    Grab pos -> acc + baseMoveCost * (1 - 2 * kidsAround pos) + extraCost
    Clean pos -> acc
    Move pos -> acc + baseMoveCost + extraCost
    _ -> error ("Unexpected movement type: " ++ show move)
  where
    -- Helpers
    baseMoveCost = Natural 100
    kidsAround position = Natural (length (filter ([Kid] ==) (adyacentsTo board position)))
    avg = Natural ((maxRows board + maxCols board) `div` 2)
    extraCost =
      let baseVal =
            if isJust target
              then distance ((position . fromJust) target) (value move)
              else 0
       in Natural (baseVal * 50)

-- Same as Balance, but avoids blocking the cribs
balanceCribCalc :: Board -> Natural -> Action Position -> Maybe Object -> HashMap Position Natural -> Natural
balanceCribCalc board acc mov@(Drop pos) target valueMaps
  | fromJust (board ! pos) == [Crib, Robot (Just Kid)] =
    let fCost = balanceCalc board acc mov target
        pCost = fromJust (Map.lookup pos valueMaps)
     in fCost - (pCost * baseCost)
  | otherwise = balanceCalc board acc mov target
  where
    baseCost = 200
    totalCribs = Natural (length (getByType board Crib))
balanceCribCalc board acc mov target _ = balanceCalc board acc mov target

fillValues board (BalanceCrib _) =
  let cribs = getByType board Crib
      costs = f cribs
      map = Map.fromList costs
   in BalanceCrib (Just map)
  where
    f [] = []
    f (c : cs) = val `par` (vals `pseq` (pos, val) : vals)
      where
        -- recursion
        vals = f cs
        -- logic
        pos = position c
        val =
          findFirstNonCrib
            board
            (Seq.fromList [(pos + dir, 0) | dir <- directions])
            Set.empty
fillValues _ calctype = calctype

findFirstNonCrib _ Seq.Empty _ = Infinite
findFirstNonCrib board ((pos, cost) :<| queue) visited
  | pos `Set.member` visited
      || isNothing maybeCell =
    findFirstNonCrib board queue visited
  | Crib `elem` cell && Kid `notElem` cell = findFirstNonCrib board newQueue newVisited
  | Crib `notElem` cell && Obstacle `notElem` cell = cost
  | otherwise = findFirstNonCrib board queue newVisited
  where
    -- store new position
    newVisited = pos `Set.insert` visited
    -- add next possible possitions
    newQueue =
      foldl'
        (\acc val -> acc :|> (val, cost + 1))
        queue
        [pos + dir | dir <- directions]
    -- cache
    maybeCell = board ! pos
    cell = fromJust maybeCell

dummyPow num 0 = 1
dummyPow num 1 = num
dummyPow num pow = num * dummyPow num (pow - 1)

distance p1 p2 =
  let r = abs (row p1 - row p2)
      c = abs (col p1 - col p2)
   in r + c
