module Agent.Simulate where

import Agent.Logic.Pathfinding.Algorithm (localApplyMove)
import Agent.Logic.Pathfinding.Find (findTarget)
import Agent.Logic.Pathfinding.PathCalculation
import Agent.Logic.TaskHandling (assignTasks)
import Agent.Objects
import Data.Maybe (fromJust, isJust, isNothing)
import Debug.Trace (trace)
import GHC.List (foldl')
import World.Objects

agentInit :: Board -> [Agent]
agentInit board = [Agent robot Nothing | robot <- robots]
  where
    robots = [obj | obj <- elems board, typex obj `elem` [Robot Nothing, Robot (Just Kid)]]

agentSim :: PathCalcType -> Board -> [Agent] -> (Board, [Agent])
agentSim calcType board = loop board []
  where
    loop board movedAgents [] = (board, movedAgents)
    loop board movedAgents notMovedAgents =
      let notMovedAssignedAgents =
            filter
              (`elem` notMovedAgents)
              (assignTasks board (movedAgents ++ notMovedAgents) calcType)
          ( updBoard,
            updMovedAgents,
            updNotMovedAgents
            ) = moveAgents board movedAgents notMovedAssignedAgents calcType
          [l1, l2] = map length [notMovedAgents, updNotMovedAgents]
       in if l1 == l2
            then (updBoard, movedAgents ++ updMovedAgents ++ updNotMovedAgents)
            else loop updBoard (movedAgents ++ updMovedAgents) updNotMovedAgents

moveAgents :: Board -> [Agent] -> [Agent] -> PathCalcType -> (Board, [Agent], [Agent])
moveAgents board _ [] _ = (board, [], [])
moveAgents board movedAgents (ag : agents) calcType =
  if isNothing (task ag) || null actions
    then
      let unAg = unassingAgent ag
          (rBoard, rmovedAgents, rnotMovedAgents) =
            moveAgents board (unAg : movedAgents) agents calcType
       in (rBoard, rmovedAgents, unAg : rnotMovedAgents)
    else
      let (rBoard, rmovedAgents, rnotMovedAgents) =
            moveAgents updBoard (updAgent : movedAgents) agents calcType
       in (rBoard, updAgent : rmovedAgents, rnotMovedAgents)
  where
    (updBoard, updAgent) = agentApplyMove board ag act
    act = head actions
    actions =
      let path = findTarget board ag calcType
          agentlessBoard = removeActiveAgents board ag (movedAgents ++ agents)
       in if null path
            then findTarget agentlessBoard ag calcType
            else path

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
    remove = foldl' f [] (filter (/= ag) agents)
    f acc val = if (isJust . task) val then entity val : acc else acc
