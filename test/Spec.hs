import AgentSimulateSpec
import ParallelPathfindingSpec
import PathCalculationSpec
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
  hspec allKidsInCribsTest
  -- Board Simulation Specs
  hspec testRandomPicking
  hspec testBoardShuffling
  hspec testKidMovement
  -- Task Pathfinding Specs
  hspec taskReachabilityTest
  -- Agent Pathfinding Specs
  hspec agentPathfindingTest
  -- Task Handling Specs
  hspec findTaskTest
  hspec taskToMatrixTest
  hspec minimumCostTest
  hspec optimizationTest
  hspec matrixToTaskTest
  hspec assignTaskTest
  -- Agent Simulation Specs
  hspec removeActiveAgentsTest
  hspec agentApplyMoveTest
  hspec moveAgentsTest
  hspec agentSimTest
  -- Visual Tests
  hspec visualTest
  -- Path Calculation Tests
  hspec bestRewardSpec
  hspec bestRewardPathfindingSpec
  hspec noBlockCribsSpecs

-- Parallel Pathfinding Tests
-- hspec taskDetectionTest
-- hspec parallelPathfindingSpec
