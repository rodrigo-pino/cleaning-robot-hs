module Agent.Logic.Pathfinding.PathCalculation where

import Agent.Objects
import Data.Maybe (fromJust, isJust, isNothing)
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Set (Set (..))
import qualified Data.Set as Set
import Debug.Trace (trace)
import GHC.List (foldl')
import World.Objects

data PathCalcType = ShortestPath | Balance | BalanceCrib

actionCalc :: PathCalcType -> Board -> Maybe Object -> Natural -> Action Position -> Natural
actionCalc typex board target acc move =
  case typex of
    ShortestPath -> acc + 1
    Balance -> balanceCalc board acc move target
    BalanceCrib -> balanceCribCalc board acc move target

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
       in Natural (baseVal * 10)

-- Same as Balance, but avoids blocking the cribs
balanceCribCalc :: Board -> Natural -> Action Position -> Maybe Object -> Natural
balanceCribCalc board acc mov@(Drop pos) target
  | fromJust (board ! pos) == [Crib, Robot (Just Kid)] =
    let fCost = balanceCalc board acc mov target
        shDistance =
          trace
            ("doing distance thing from" ++ show pos)
            findFirstNonCrib
            (Seq.fromList [(pos + dir, 1) | dir <- directions])
            Set.empty
     in trace ("cost is " ++ show (fCost, shDistance * 300, fCost - shDistance * 300)) fCost - (shDistance * 300)
  | otherwise = balanceCalc board acc mov target
  where
    -- find shortest path
    findFirstNonCrib Seq.Empty _ = Infinite
    findFirstNonCrib ((pos, cost) :<| queue) visited
      | pos `Set.member` visited
          || isNothing maybeCell =
        findFirstNonCrib queue visited
      | Crib `elem` cell && Kid `notElem` cell = findFirstNonCrib newQueue newVisited
      | Obstacle `notElem` cell = trace ("Found clean case in" ++ show cell ++ " at: " ++ show pos) cost
      | otherwise = findFirstNonCrib queue newVisited
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
        cell = trace ("Analyzing pos: " ++ show pos) fromJust maybeCell
balanceCribCalc board acc mov target = balanceCalc board acc mov target

distance p1 p2 =
  let r = abs (row p1 - row p2)
      c = abs (col p1 - col p2)
   in r + c
