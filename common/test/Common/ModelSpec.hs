module Common.ModelSpec where

import Test.Hspec
import qualified Servant.API as Servant

import Common.Model (Model, initialModel)
import Common.View as View

constructUri :: String -> Servant.URI
constructUri path = Servant.URI
  { Servant.uriScheme    = "http:"
  , Servant.uriAuthority = Nothing
  , Servant.uriPath      = path
  , Servant.uriQuery     = ""
  , Servant.uriFragment  = ""
  }

spec :: Spec
spec = do
  describe "Model URI /" $ it "routes correctly" $ do
    let mockModel = initialModel $ constructUri "/"
    show (View.viewModel mockModel) `shouldBe` show (View.homeView mockModel)
  describe "Model URI /flipped" $ it "routes correctly" $ do
    let mockModel = initialModel $ constructUri "/flipped"
    show (View.viewModel mockModel) `shouldBe` show (View.flippedView mockModel)
