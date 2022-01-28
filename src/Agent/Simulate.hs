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
    (board1, agents1) = moveAgents originalBoard [] allAgents
    (board2, movedAgents2) = moveAgents board1 movedAgents1 notMovedAgents

    (notMovedAgents, movedAgents1) = foldl f ([], []) (zip agents1 assignedAgents)
    f (notM, m) (a1, a2) = if isNothing (task a1) then (a2 : notM, m) else (notM, a2 : m)

    assignedAgents = assignTasks board1 agents1

moveAgents :: Board -> [Agent] -> [Agent] -> (Board, [Agent])
moveAgents board _ [] = (board, [])
moveAgents board movedAgents (ag : agents) =
  if isNothing (task ag)
    then
      let (rBoard, rAgents) = moveAgents board (ag : movedAgents) agents
       in (rBoard, ag : rAgents)
    else
      let (rBoard, rAgents) = moveAgents updBoard (updAgent : movedAgents) agents
       in (rBoard, updAgent : rAgents)
  where
    (updBoard, updAgent) =
      if null actions
        then (board, unassingAgent ag)
        else agentApplyMove board ag act
    actions =
      let path = pathToTask board ag objective
          agentlessBoard = removeActiveAgents board ag (movedAgents ++ agents)
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
        updTask = case (action, (position . fromJust . getTask) agent) of
          (Clean x, y)
            | x == y -> Nothing
            | otherwise -> task agent
          (Drop x, y)
            | x == y -> Nothing
            | otherwise -> task agent
          _ -> task agent
     in (updBoard, Agent updObj updTask)
  where
    objs = fromJust (board ! value action)
    robotBlock =
      value action /= (position . entity) agent
        && ( Robot Nothing `elem` objs
               || Robot (Just Kid) `elem` objs
           )

removeActiveAgents :: Board -> Agent -> [Agent] -> Board
removeActiveAgents board ag agents = board *-- remove
  where
    -- get all robots for possible removoal, except the one of the agent in question
    remove = foldl f [] (filter (/= ag) agents)
    f acc val = if (isJust . task) val then entity val : acc else acc
