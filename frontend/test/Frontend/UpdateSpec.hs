module Frontend.UpdateSpec where

import Test.Hspec

import Common.Model (Model, initialModel)
import Frontend.Update (updateModel)

spec :: Spec
spec = do
  describe "Just a blank test"
    $
    -- TODO: Test `updateModel` or similar.
      it "just works..."
    $ do
        "Not implemented yet" `shouldBe` "Not implemented yet"
