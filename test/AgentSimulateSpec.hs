module AgentSimulateSpec where

import Agent.Objects
import Agent.Simulate
import Test.Hspec
import Test.Tasty
import Test.Tasty.Hspec
import World.Board
import World.Objects
import World.Simulate

removeActiveAgentsTest :: SpecWith ()
removeActiveAgentsTest = describe "Remove robots owned by active agents" $ do
  it "Should remove all active agents" $
    let dirt = make Dirt (2, 2)
        r1@[robot1, robot2] = makeMany (Robot Nothing) [(0, 0), (0, 1)]
        r2@[robot3, robot4] = makeMany (Robot (Just Kid)) [(0, 2), (0, 3)]
        a1@[ag1, ag3] = [mockAgent robi [] dirt | robi <- [robot1, robot3]]
        a2@[ag2, ag4] = [mockAgent robi [Move (Position 1 0)] dirt | robi <- [robot2, robot4]]
        board = newBoard 2 4
        resultBoard = removeActiveAgents (board *++ r1 *++ r2) (a1 ++ a2)
        expectedBoard = board *++ [robot2, robot4]
     in resultBoard `shouldBe` expectedBoard

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
        (expectedBoard, expectedAgent) = (board *++ [mainR, crib, kid], mockAgent mainR [move] crib)
     in do
          updatedAgent `shouldBe` expectedAgent
          updatedBoard `shouldBe` expectedBoard
  it "Should grab the kid" $
    let move = Grab (Position 2 1)
        kid = make Kid (2, 1)
        carryR = make (Robot (Just Kid)) (2, 1)
        agent = mockAgent mainR [move] kid
        (updatedBoard, updatedAgent) = agentApplyMove (board *++ [mainR, kid]) agent move
        (expectedBoard, expectedAgent) = (board *++ [carryR], mockAgent carryR [move] kid)
     in do
          updatedAgent `shouldBe` expectedAgent
          updatedBoard `shouldBe` expectedBoard
  where
    mainR = make (Robot Nothing) (2, 2)
    board = newBoard 4 4

moveAgentsTest :: SpecWith ()
moveAgentsTest = describe "Move all posible agents" $ do
  it "Should move the agents" $
    let x = 3
     in 3 `shouldBe` 3
  it "Should move the agents and made another missionles" $
    let x = 3
     in 3 `shouldBe` 3

agentSimTest :: SpecWith ()
agentSimTest = describe "Agents should do all tasks" $ do
  it "Should move the agents do the tasks #1" $
    let x = 3
     in 3 `shouldBe` 3
  it "Should move the agents to the tasks #2" $ do
    let x = 3
     in 3 `shouldBe` 3

mockAgent :: Object -> [Action Position] -> Object -> Agent
mockAgent obj [] _ = Agent obj Nothing
mockAgent obj actions targetx = Agent obj (Just (AssignedTask targetx actions))
