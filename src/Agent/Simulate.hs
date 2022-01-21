module Agent.Simulate where

import Agent.Logic.Pathfinding (localApplyMove)
import Agent.Objects
import Data.Maybe (isNothing)
import World.Objects

initAgents :: Board -> [Agent]
initAgents board = [Agent robot Nothing | robot <- robots]
  where
    robots = [obj | obj <- elems board, typex obj `elem` [Robot Nothing, Robot (Just Kid)]]

handleAgents :: Board -> [Agent] -> (Board, [Agent])
handleAgents board agents = (board, agents)
  where
    (updatedBoard, updatedAgents) = moveAgents board agents
    missionLess = [ag | ag <- updatedAgents, isNothing (task ag)]

moveAgents :: Board -> [Agent] -> (Board, [Agent])
moveAgents board [] = (board, [])
moveAgents board (ag : agents) =
  let (restulBoard, resultAgents) = moveAgents updBoard agents
   in (restulBoard, updAgent : resultAgents)
  where
    action = Move (Position 1 1) --recalculate path from agent to target
    updAgent = ag
    updBoard = case task ag of
      Nothing -> board
      _ -> board

agentApplyMove :: Board -> Agent -> Action Position -> (Board, Agent)
agentApplyMove board agent action = (updBoard, Agent updObj (task agent))
  where
    oldObj = entity agent
    (updObj, updBoard) = localApplyMove board oldObj action
