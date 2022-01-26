module Agent.Simulate where

import Agent.Logic.Pathfinding (localApplyMove, pathToTask)
import Agent.Logic.TaskHandling (assignTasks)
import Agent.Objects
import Data.Maybe (fromJust, isJust, isNothing)
import Debug.Trace (trace)
import World.Objects

agentInit :: Board -> [Agent]
agentInit board = [Agent robot Nothing | robot <- robots]
  where
    robots = [obj | obj <- elems board, typex obj `elem` [Robot Nothing, Robot (Just Kid)]]

agentSim :: Board -> [Agent] -> (Board, [Agent])
agentSim originalBoard allAgents = (board2, movedAgents1 ++ movedAgents2)
  where
    (board1, agents1) = moveAgents allAgents originalBoard allAgents
    (board2, movedAgents2) = moveAgents allAgents board1 notMovedAgents

    (notMovedAgents, movedAgents1) = foldl f ([], []) (zip agents1 assignedAgents)
    f (notM, m) (a1, a2) = if isNothing (task a1) then (a2 : notM, m) else (notM, a2 : m)

    assignedAgents = assignTasks board1 agents1

moveAgents :: [Agent] -> Board -> [Agent] -> (Board, [Agent])
moveAgents _ board [] = (board, [])
moveAgents allAgents board (ag : agents) =
  if isNothing (task ag)
    then
      let (rBoard, rAgents) = moveAgents allAgents board agents
       in (rBoard, ag : rAgents)
    else
      let (rBoard, rAgents) = moveAgents allAgents updBoard agents
       in (rBoard, updAgent : rAgents)
  where
    (updBoard, updAgent) =
      if null actions
        then (board, unassingAgent ag)
        else agentApplyMove board ag act -- remove missionless agents
    actions =
      let path = pathToTask board ag objective
          agentlessBoard = removeActiveAgents board allAgents
       in if null path
            then pathToTask agentlessBoard ag objective
            else path
    objective = fromJust (getTask ag)
    act = head actions

agentApplyMove :: Board -> Agent -> Action Position -> (Board, Agent)
agentApplyMove board agent action
  | robotBlock = (board, agent)
  | otherwise =
    let (updObj, updBoard) = localApplyMove board (entity agent) action
     in (updBoard, Agent updObj (task agent))
  where
    objs = fromJust (board ! value action)
    robotBlock =
      value action /= (position . entity) agent
        && ( Robot Nothing `elem` objs
               || Robot (Just Kid) `elem` objs
           )

removeActiveAgents :: Board -> [Agent] -> Board
removeActiveAgents board agents = board *-- remove
  where
    robots = getByTypes board [Robot Nothing, Robot (Just Kid)]
    remove = foldl f [] robots
    f acc val = if hasMission val then acc else val : acc
    hasMission robot = any (\a -> entity a == robot && (isJust . task) a) agents
