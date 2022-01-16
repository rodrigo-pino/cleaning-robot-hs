module Agent.Logic.TaskHandling where

import Agent.Objects
import Data.List (elemIndex)
import qualified Data.Matrix as M
import qualified GHC.Arr as A
import World.Objects

getTasks :: Board -> [Task]
getTasks board = [Task {target = obj, solvers = []} | obj <- elems board, typex obj `elem` tasks]
  where
    tasks = [Dirt, Crib]

-- Need path finding operational to complete this method!!!
taskHandler :: [Task] -> [Agent] -> [(Int, Int)]
taskHandler tasks agents =
  optimize
    (getCostMatrix tasks agents)
  where
    costMatrix = getCostMatrix tasks agents
    rawTaskAssignment = optimize costMatrix
    res = foldl (\acc val -> acc ++ [val]) [] rawTaskAssignment

-- returns list of tuples (i, j) which means task i will be done by agent j
optimize :: M.Matrix (Natural, [(Int, Int)]) -> [(Int, Int)]
optimize costMatrix =
  let minMatrix = dp A.! (m - 1)
      (_, minInd) = minimumCost minMatrix (-1, -1)
   in snd (minMatrix M.! minInd)
  where
    n = M.nrows costMatrix
    m = M.ncols costMatrix

    dp = A.listArray (0, m) [f k | k <- [0 .. m - 1]]
    f :: Int -> M.Matrix (Natural, [(Int, Int)])
    f 0 = costMatrix
    f k =
      M.fromList
        n
        m
        [ (mc, (i, j) : tail)
          | i <- [0 .. n - 1],
            j <- [0 .. m - 1],
            let (mc, agent) = minimumCost prevMatrix (i, j),
            let tail = snd (prevMatrix M.! agent)
        ]
      where
        prevMatrix = dp A.! (k - 1)

type Acc = ((Natural, (Int, Int)), (Int, Int))

minimumCost :: M.Matrix (Natural, [(Int, Int)]) -> (Int, Int) -> (Natural, (Int, Int))
minimumCost costMatrix currentIndex = fst (foldl f ((Infinite, (-1, -1)), (0, 0)) costMatrix)
  where
    n = M.nrows costMatrix
    m = M.ncols costMatrix

    f :: Acc -> (Natural, [(Int, Int)]) -> Acc
    f ((minVal, minInd), ind) (nat, ignore)
      | nat < minVal && currentIndex `notElem` ind : ignore = ((nat, ind), incIndex ind)
      | otherwise = ((minVal, minInd), incIndex ind)

    incIndex :: (Int, Int) -> (Int, Int)
    incIndex (i, j)
      | i == n = error "Index out of range while searching for minimum"
      | j == m - 1 = (i + 1, 0)
      | otherwise = (i, j + 1)

getCostMatrix :: [Task] -> [Agent] -> M.Matrix (Natural, [(Int, Int)])
getCostMatrix tasks agents =
  M.fromList
    n
    m
    [ (time solver, [(i, j)])
      | (i, task) <- zip [0 .. n - 1] tasks,
        (j, solver) <- zip [0 .. m - 1] (completeSolvers (solvers task) agents)
    ]
  where
    n = length tasks
    m = length agents

completeSolvers :: [Solver] -> [Agent] -> [Solver]
completeSolvers solvers allAgents =
  [ Solver agentx completionTime
    | agentx <- allAgents,
      let completionTime = getCompletionTime solvers agentx
  ]

getCompletionTime :: [Solver] -> Agent -> Natural
getCompletionTime [] _ = Infinite
getCompletionTime (solver : solvers) agentx
  | agent solver == agentx = time solver
  | otherwise = getCompletionTime solvers agentx
