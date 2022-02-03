module Agent.Simulate where

import Agent.Logic.Pathfinding.Algorithm (localApplyMove)
import Agent.Logic.Pathfinding.Find (findTarget, findTargets)
import Agent.Logic.Pathfinding.PathCalculation
import Agent.Logic.TaskHandling (assignTasks)
import Agent.Objects
import Control.Parallel (par, pseq)
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
       in if notMovedAgents %== updNotMovedAgents
            then (updBoard, updMovedAgents ++ updNotMovedAgents)
            else loop updBoard updMovedAgents updNotMovedAgents

moveAgents :: Board -> [Agent] -> [Agent] -> PathCalcType -> (Board, [Agent], [Agent])
moveAgents board movedAgents toMoveAgents calcType =
  (newBoard, movedAgents ++ movedAg, notMovedAg)
  where
    -- Apply agents actions sequentially
    (newBoard, movedAg, notMovedAg) = g board toMoveAgents paths
    g board [] [] = (board, [], [])
    g board (ag : ags) (path : paths)
      | null path =
        let (resBoard, resMovedAg, resNotMovedAg) = g board ags paths
         in (resBoard, resMovedAg, unassingAgent ag : resNotMovedAg)
      | otherwise =
        let (resBoard, resMovedAg, resNotMovedAg) = g updBoard ags paths
         in (resBoard, updAg : resMovedAg, resNotMovedAg)
      where
        action = head path
        (updBoard, updAg) = agentApplyMove board ag action

    -- Finding path for each agent and its objective using parallel execution
    paths = f toMoveAgents -- findTargets board toMoveAgents calcType
    f [] = []
    f (ag : ags)
      | (isNothing . task) ag = [] : ps
      | otherwise = p `par` (ps `pseq` p : ps)
      where
        ps = f ags
        agentlessBoard = removeActiveAgents board ag allAgents
        p = findTarget agentlessBoard ag calcType

    -- helpers
    allAgents = movedAgents ++ toMoveAgents

moveAgentsOld :: Board -> [Agent] -> [Agent] -> PathCalcType -> (Board, [Agent], [Agent])
moveAgentsOld board _ [] _ = (board, [], [])
moveAgentsOld board movedAgents (ag : agents) calcType =
  if isNothing (task ag) || null actions
    then
      let unAg = unassingAgent ag
          (rBoard, rmovedAgents, rnotMovedAgents) =
            moveAgentsOld board (unAg : movedAgents) agents calcType
       in (rBoard, rmovedAgents, unAg : rnotMovedAgents)
    else
      let (rBoard, rmovedAgents, rnotMovedAgents) =
            moveAgentsOld updBoard (updAgent : movedAgents) agents calcType
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
