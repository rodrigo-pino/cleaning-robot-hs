module Agent.Logic.TaskHandling where

import Agent.Logic.Pathfinding
import Agent.Objects
import Data.List (elemIndex)
import qualified Data.Matrix as M
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust, isNothing, mapMaybe)
import Debug.Trace (trace)
import qualified GHC.Arr as A
import World.Objects

-- Minimum value and its position, current coordinates
type Acc = ((Natural, (Int, Int)), (Int, Int))

assignTasks :: Board -> [Agent] -> [Agent]
assignTasks board agents = getOptimalTaskDivison board tasks agents
  where
    tasks = findSolvers board (getTasks board agents) agents

getTasks :: Board -> [Agent] -> [Task]
getTasks board agents = [tsk | tsk <- tasks, target tsk `notElem` takenTasks]
  where
    tasks =
      [ Task {target = obj, solvers = []}
        | obj <- elems board,
          typex obj `elem` [Dirt, Crib]
      ]
    takenTasks =
      [ (destinaton . fromJust) assignedTask
        | agt <- agents,
          let assignedTask = task agt,
          isJust assignedTask
      ]

getOptimalTaskDivison :: Board -> [Task] -> [Agent] -> [Agent]
getOptimalTaskDivison board tasks agents
  | null rawTaskAssignment = agents
  | otherwise = parseTaskDivision board tasks agents rawTaskAssignment
  where
    costMatrix = getCostMatrix tasks agents
    rawTaskAssignment = optimize costMatrix

-- returns list of tuples (i, j) which means task i will be done by agent j
optimize :: M.Matrix (Natural, [(Int, Int)]) -> [(Int, Int)]
optimize costMatrix = fromMaybe [] (foldr extract Nothing dp)
  where
    -- Scans all the matrices produced for the best agent combination
    -- It tries using all, then all - 1, all - 2, etc ...
    -- This is done for when only a subset of the agents can be used
    extract val acc
      | isNothing acc =
        let (nat, minInd) = minimumCost val (-1, -1)
         in case nat of
              Natural _ -> Just (snd (val M.! valid minInd))
              Infinite -> Nothing
      | otherwise = acc

    -- Dynamic programming logic
    dp = [f k | k <- [0 .. m - 1]]
    f :: Int -> M.Matrix (Natural, [(Int, Int)])
    f 0 = costMatrix
    f k =
      M.fromList
        n
        m
        [ (costij + mc, (i, j) : tail)
          | let prevMatrix = dp !! (k - 1),
            i <- [0 .. n - 1],
            j <- [0 .. m - 1],
            let (mc, agent) = minimumCost prevMatrix (i, j),
            let costij = fst (costMatrix M.! valid (i, j)),
            let tail = snd (prevMatrix M.! valid agent)
        ]

    -- Helpers!!!
    n = M.nrows costMatrix
    m = M.ncols costMatrix
    valid (i, j) = (i + 1, j + 1)

minimumCost :: M.Matrix (Natural, [(Int, Int)]) -> (Int, Int) -> (Natural, (Int, Int))
minimumCost costMatrix currentIndex = fst (foldl f ((Infinite, (-1, -1)), (0, 0)) costMatrix)
  where
    n = M.nrows costMatrix
    m = M.ncols costMatrix

    f :: Acc -> (Natural, [(Int, Int)]) -> Acc
    f ((minVal, minInd), ind) (nat, ignore)
      | nat < minVal
          && (currentIndex `hardNotEq` (ind : ignore)) =
        ((nat, ind), incIndex ind)
      | otherwise = ((minVal, minInd), incIndex ind)

    incIndex :: (Int, Int) -> (Int, Int)
    incIndex (i, j)
      | i == n = error "Index out of range while searching for minimum"
      | j == m - 1 = (i + 1, 0)
      | otherwise = (i, j + 1)

    hardNotEq v [] = True
    hardNotEq v (x : xs) = fst v /= fst x && snd v /= snd x && hardNotEq v xs

-- Get tasks agents and how they will be split, and returns new tasks
parseTaskDivision :: Board -> [Task] -> [Agent] -> [(Int, Int)] -> [Agent]
parseTaskDivision board tasks agents assignation =
  zipWith (curry assign) [0 .. length agents - 1] agents
  where
    assign (i, agent)
      | isJust assignedTask || taskIndex == -1 = agent
      | otherwise =
        Agent
          { entity = entity agent,
            task =
              Just
                AssignedTask
                  { destinaton = target designatedTask,
                    actions = path
                  }
          }
      where
        assignedTask = task agent
        taskIndex = searchTask i assignation
        designatedTask = tasks !! taskIndex
        path = pathToTask board agent (target designatedTask)

    searchTask i [] = -1
    searchTask i (x : xs)
      | snd x == i = fst x
      | otherwise = searchTask i xs

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
