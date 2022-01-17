import PathfindingSpec
import TaskHandlingSpec
import Test.Hspec (hspec)
import VisualSpec
import WorldSpec

main :: IO ()
main = do
  -- Board Rules Tests
  hspec boardPrimitiveTest
  hspec kidTest
  hspec robotWithoutKidTest
  hspec robotWithKidTest
  hspec actionApplicationTest
  -- Task Pathfinding Specs
  hspec taskReachabilityTest
  hspec taskDetectionTest
  -- Agent Pathfinding Specs
  hspec agentPathfindingTest
  -- Visual Tests
  hspec visualTest
