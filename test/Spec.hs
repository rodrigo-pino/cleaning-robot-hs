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
  -- Task Specs
  hspec taskReachabilityTest
  hspec taskDetecttionTest
  -- Visual Tests
  hspec visualTest
