import Test.Hspec

import qualified Frontend.UpdateSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Frontend.UpdateSpec" Frontend.UpdateSpec.spec
