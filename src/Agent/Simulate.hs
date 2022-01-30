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
agentSim board = loop board []
  where
    loop board movedAgents [] = (board, movedAgents)
    loop board movedAgents notMovedAgents =
      let notMovedAssignedAgents =
            filter
              (`elem` notMovedAgents)
              (assignTasks board (movedAgents ++ notMovedAgents))
          ( updBoard,
            updMovedAgents,
            updNotMovedAgents
            ) = moveAgents board movedAgents notMovedAssignedAgents
          [l1, l2] = map length [notMovedAgents, updNotMovedAgents]
       in if l1 == l2
            then (updBoard, movedAgents ++ updMovedAgents ++ updNotMovedAgents)
            else loop updBoard (movedAgents ++ updMovedAgents) updNotMovedAgents

moveAgents :: Board -> [Agent] -> [Agent] -> (Board, [Agent], [Agent])
moveAgents board _ [] = (board, [], [])
moveAgents board movedAgents (ag : agents) =
  if isNothing (task ag) || null actions
    then
      let unAg = unassingAgent ag
          (rBoard, rmovedAgents, rnotMovedAgents) =
            moveAgents board (unAg : movedAgents) agents
       in (rBoard, rmovedAgents, unAg : rnotMovedAgents)
    else
      let (rBoard, rmovedAgents, rnotMovedAgents) =
            moveAgents updBoard (updAgent : movedAgents) agents
       in (rBoard, updAgent : rmovedAgents, rnotMovedAgents)
  where
    (updBoard, updAgent) = agentApplyMove board ag act
    act = head actions
    actions =
      let path = pathToTask board ag objective
          agentlessBoard = removeActiveAgents board ag (movedAgents ++ agents)
       in if null path
            then pathToTask agentlessBoard ag objective
            else path
    objective = fromJust (getTask ag)

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
