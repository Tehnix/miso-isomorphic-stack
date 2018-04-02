module Main where

import App (app)
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  with (return app) $
  describe "GET /" $ do
    it "responds with 200" $ get "/" `shouldRespondWith` 200
    it "responds with 'Simple'" $ get "/" `shouldRespondWith` "Simple"
