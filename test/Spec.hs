import Test.Hspec (hspec)
import VisualSpec
import WorldSpec

main :: IO ()
main = do
  hspec boardPrimitiveTest
  hspec kidTest
  hspec robotWithoutKidTest
  hspec robotWithKidTest
  hspec visualTest
