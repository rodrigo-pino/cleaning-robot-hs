import AgentSimulateSpec
import PathfindingSpec
import TaskHandlingSpec
import Test.Hspec (hspec)
import VisualSpec
import WorldSimulateSpec
import WorldSpec

main :: IO ()
main = do
  -- Board Rules Specs
  hspec boardPrimitiveTest
  hspec kidTest
  hspec robotWithoutKidTest
  hspec robotWithKidTest
  hspec actionApplicationTest
  hspec kidClusterDetection
  -- Board Simulation Specs
  hspec testRandomPicking
  hspec testBoardShuffling
  hspec testKidMovement
  -- Task Pathfinding Specs
  hspec taskReachabilityTest
  hspec taskDetectionTest
  -- Agent Pathfinding Specs
  hspec agentPathfindingTest
  -- Task Handling Specs
  hspec findTaskTest
  hspec taskToMatrixTest
  hspec minimumCostTest
  hspec optimizationTest
  hspec matrixToTaskTest
  -- Agent Simulation Specs
  hspec removeActiveAgentsTest
  hspec agentApplyMoveTest
  hspec moveAgentsTest
  -- Visual Tests
  hspec visualTest
