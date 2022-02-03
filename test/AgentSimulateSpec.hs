module AgentSimulateSpec where

import Agent.Logic.Pathfinding.PathCalculation (PathCalcType (ShortestPath))
import Agent.Objects
import Agent.Simulate
import Data.Maybe (isNothing)
import Debug.Trace (trace)
import Test.Hspec
import Test.Tasty
import Test.Tasty.Hspec
import Visual (printState)
import World.Board
import World.Objects
import World.Simulate

removeActiveAgentsTest :: SpecWith ()
removeActiveAgentsTest =
  describe "Remove robots owned by active agents" $
    it "Should remove all active agents" $
      let dirt = make Dirt (2, 2)
          r1@[robot1, robot2] = makeMany (Robot Nothing) [(0, 0), (0, 1)]
          r2@[robot3, robot4] = makeMany (Robot (Just Kid)) [(0, 2), (0, 3)]
          a1@[ag1, ag3] = [mockAgent robi [] dirt | robi <- [robot1, robot3]]
          a2@[ag2, ag4] = [mockAgent robi [Move (Position 1 0)] dirt | robi <- [robot2, robot4]]
          board = newBoard 2 4
          resultBoard1 = removeActiveAgents (board *++ r1 *++ r2) ag2 (a1 ++ a2)
          expectedBoard1 = board *++ [robot1, robot2, robot3]
       in do
            resultBoard1 `shouldBe` expectedBoard1

agentApplyMoveTest :: SpecWith ()
agentApplyMoveTest = describe "Move an agent through the board" $ do
  it "Should move the agent" $
    let move = Move (Position 2 1)
        dirt = make Dirt (0, 2)
        agent = mockAgent mainR [move] dirt
        (updatedBoard, updatedAgent) = agentApplyMove (board *+ mainR) agent move
        expectedR = make (Robot Nothing) (2, 1)
        (expectedBoard, expectedAgent) = (board *+ expectedR, mockAgent expectedR [move] dirt)
     in do
          updatedBoard `shouldBe` expectedBoard
          updatedAgent `shouldBe` expectedAgent
  it "Should not move the agent when is blocked by robots" $
    let move = Move (Position 2 1)
        dirt = make Dirt (0, 2)
        agent = mockAgent mainR [move] dirt
        blockade = makeMany (Robot Nothing) [(3, 2), (2, 3), (1, 2), (2, 1)]
        (updatedBoard, updatedAgent) = agentApplyMove (board *++ (mainR : blockade)) agent move
        (expectedBoard, expectedAgent) = (board *++ (mainR : blockade), mockAgent mainR [move] dirt)
     in do
          updatedBoard `shouldBe` expectedBoard
          updatedAgent `shouldBe` expectedAgent
  it "Should not block himself" $
    let move = Clean (Position 2 2)
        dirt = make Dirt (2, 2)
        agent = mockAgent mainR [move] dirt
        (updatedBoard, updatedAgent) = agentApplyMove (board *++ [mainR, dirt]) agent move
        (expectedBoard, expectedAgent) = (board *+ mainR, mockAgent mainR [move] dirt)
     in do
          updatedBoard `shouldBe` expectedBoard
          updatedAgent `shouldBe` expectedAgent
  it "Should drop the kid" $
    let move = Drop (Position 2 2)
        crib = make Crib (2, 2)
        kid = make Kid (2, 2)
        carryR = make (Robot (Just Kid)) (2, 2)
        agent = mockAgent carryR [move] crib
        (updatedBoard, updatedAgent) = agentApplyMove (board *++ [carryR, crib]) agent move
        (expectedBoard, expectedAgent) = (board *++ [mainR, crib, kid], Agent mainR Nothing)
     in do
          updatedAgent `shouldBe` expectedAgent
          updatedBoard `shouldBe` expectedBoard
  it "Should grab the kid" $
    let move = Grab (Position 2 1)
        kid = make Kid (2, 1)
        crib = make Crib (2, 2)
        carryR = make (Robot (Just Kid)) (2, 1)
        agent = mockAgent mainR [move] crib
        (updatedBoard, updatedAgent) = agentApplyMove (board *++ [mainR, kid]) agent move
        (expectedBoard, expectedAgent) = (board *++ [carryR], mockAgent carryR [move] crib)
     in do
          updatedAgent `shouldBe` expectedAgent
          updatedBoard `shouldBe` expectedBoard
  it "Should unassing the agent after mission completion" $
    let dirt = make Dirt (2, 2)
        move = Clean (Position 2 2)
        agent = mockAgent mainR [move] dirt
        (updBoard, updAg) = agentApplyMove (board *++ [mainR, dirt]) agent move
        (expBoard, expAg) = (board *++ [mainR], Agent mainR Nothing)
     in do
          updBoard `shouldBe` expBoard
          updAg `shouldBe` expAg
          (isNothing . task) updAg `shouldBe` True
  where
    mainR = make (Robot Nothing) (2, 2)
    board = newBoard 4 4

moveAgentsTest :: SpecWith ()
moveAgentsTest = describe "Move all posible agents" $ do
  it "Should move the agent" $
    let dirt = make Dirt (0, 3)
        [robot, expectedRobot] = makeMany (Robot Nothing) [(0, 0), (0, 1)]
        move = Move (Position 0 1)
        ag = mockAgent robot [move] dirt
        board = newBoard 1 4 *++ [dirt, robot]
        (resultBoard, movedAgents, notMovedAgents) = moveAgents board [] [ag] calcType
        expectedBoard = newBoard 1 4 *++ [dirt, expectedRobot]
        expectedAgents = [mockAgent expectedRobot [move] dirt]
        expectedNotAgents = []
     in do
          notMovedAgents `shouldBe` expectedNotAgents
          movedAgents `shouldBe` expectedAgents
          resultBoard `shouldBe` expectedBoard

  it "Should move all the agents" $
    let dirts = makeMany Dirt [(0, 0), (0, 2), (0, 4)]
        robots = makeMany (Robot Nothing) [(4, 0), (4, 2), (4, 4)]
        actions = [Move (Position 3 (2 * i)) | i <- [0 .. 2]]
        agents = [mockAgent obj [move] t | (obj, move, t) <- zip3 robots actions dirts]
        board = newBoard 5 5 *++ (dirts ++ robots)
        (resultBoard, movedAgents, notMovedAgents) = moveAgents board [] agents calcType

        expectedRobots = makeMany (Robot Nothing) [(3, 0), (3, 2), (3, 4)]
        expectedAgents =
          [ mockAgent obj [move] t
            | (obj, move, t) <- zip3 expectedRobots actions dirts
          ]
        expectedBoard = newBoard 5 5 *++ (dirts ++ expectedRobots)
     in do
          movedAgents `shouldBe` expectedAgents
          notMovedAgents `shouldBe` []
          resultBoard `shouldBe` expectedBoard
  it "Should remove tasks from blocked ones" $
    let obstacles = makeMany Obstacle [(3, 2), (2, 3), (1, 2), (2, 1)]
        dirt = make Dirt (0, 0)
        move = Move (Position 2 1)
        robot = make (Robot Nothing) (2, 2)
        agent = mockAgent robot [move] dirt
        board = newBoard 5 5 *++ (dirt : robot : obstacles)
        (resultBoard, [], [resultAgent]) = moveAgents board [] [agent] calcType
        expectedAgent = mockAgent robot [] dirt
     in do
          resultAgent `shouldBe` expectedAgent
          resultBoard `shouldBe` board

agentSimTest :: SpecWith ()
agentSimTest = describe "Agents should do all tasks" $ do
  it "Should move the agents do the tasks #1" $
    let dirts = makeMany Dirt [(0, i * 2) | i <- [0 .. 2]]
        robots = makeMany (Robot Nothing) [(4, i * 2) | i <- [0 .. 2]]
        board = newBoard 5 5 *++ (dirts ++ robots)
        agents = agentInit board
        (resultboard, resultAgent) = loop 5 (board, agents)
     in getByType resultboard Dirt `shouldBe` []
  it "Should move the agents to the tasks #2" $
    let dirts@[dirt1, dirt2, dirt3] = makeMany Dirt [(4, 0), (2, 2), (0, 4)]
        robots = makeMany (Robot Nothing) [(6, i * 2) | i <- [0 .. 2]]
        board = newBoard 7 5 *++ (dirts ++ robots)
        agents = agentInit board
        (resultBoard1, resultAgent1) = loop 3 (board, agents)
        (resultBoard2, resultAgent2) = loop 3 (resultBoard1, resultAgent1)
        (resultBoard3, resultAgent3) = loop 3 (resultBoard2, resultAgent2)
     in do
          getByType resultBoard1 Dirt `shouldBe` [dirt2, dirt3]
          getByType resultBoard2 Dirt `shouldBe` [dirt3]
          getByType resultBoard3 Dirt `shouldBe` []
  it "Should move the agents to do the tasks #3" $
    let obstacles = makeMany Obstacle [(i, j) | i <- [1 .. 4], j <- [0, 1, 3, 4]]
        dirts@[dirt1, dirt2, dirt3] = makeMany Dirt [(0, j) | j <- [0, 2, 4]]
        robots = makeMany (Robot Nothing) [(5, j) | j <- [0, 2, 4]]
        board = newBoard 6 5 *++ (obstacles ++ dirts ++ robots)
        agents = agentInit board
        (resultBoard1, resultAgent1) = loop 6 (board, agents)
        (resultBoard2, resultAgent2) = loop 3 (resultBoard1, resultAgent1)
        (resultBoard3, resultAgent3) = loop 2 (resultBoard2, resultAgent2)
     in do
          getByType resultBoard1 Dirt `shouldBe` [dirt1, dirt3]
          getByType resultBoard2 Dirt `shouldBe` [dirt1]
          getByType resultBoard3 Dirt `shouldBe` []
  it "Should move the agents to do the tasks #4" $
    let obstacles = makeMany Obstacle [(i, j) | i <- [1, 2, 3], j <- [0, 1, 3, 4]]
        dirts@[dirt1, dirt2, dirt3] = makeMany Dirt [(0, j) | j <- [0, 2, 4]]
        robots = makeMany (Robot Nothing) [(5, j) | j <- [0, 2, 4]]
        board = newBoard 6 5 *++ (obstacles ++ dirts ++ robots)
        agents = agentInit board
        (resultBoard1, resultAgent1) = loop 6 (board, agents)
        (resultBoard2, resultAgent2) = loop 3 (resultBoard1, resultAgent1)
        (resultBoard3, resultAgent3) = loop 2 (resultBoard2, resultAgent2)
     in do
          getByType resultBoard1 Dirt `shouldBe` [dirt1, dirt3]
          getByType resultBoard2 Dirt `shouldBe` [dirt1]
          getByType resultBoard3 Dirt `shouldBe` []
  it "Should move the agents to the tasks #5" $
    let robots = makeMany (Robot Nothing) [(0, i) | i <- [0 .. 3]]
        dirts = makeMany Dirt [(0, i) | i <- [4 .. 7]]
        board = newBoard 1 8 *++ (robots ++ dirts)
        agents = agentInit board
        (resultBoard1, resultAgent1) = loop 3 (board, agents)
        (resultBoard2, resultAgent2) = loop 3 (resultBoard1, resultAgent1)
        (resultBoard3, resultAgent3) = loop 3 (resultBoard2, resultAgent2)
     in do
          printState resultBoard1
          print resultAgent1
          printState resultBoard2
          print resultAgent2
          printState resultBoard3
          print resultAgent3

          getByType resultBoard1 Dirt `shouldNotBe` dirts
          getByType resultBoard2 Dirt `shouldNotBe` []
  where
    loop 0 r = r
    loop num (board, agents) = loop (num - 1) (agentSim calcType board agents)

mockAgent :: Object -> [Action Position] -> Object -> Agent
mockAgent obj [] _ = Agent obj Nothing
mockAgent obj actions targetx = Agent obj (Just (AssignedTask targetx actions))

calcType = ShortestPath
