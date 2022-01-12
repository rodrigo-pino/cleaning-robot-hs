import Test.Hspec (hspec)
import WorldSpec

main :: IO ()
main = do
  hspec boardPrimitiveTest
  hspec kidTest
  hspec robotWithoutKidTest
  hspec robotWithKidTest
