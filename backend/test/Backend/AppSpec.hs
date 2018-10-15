module Backend.AppSpec where

import Test.Hspec
import Test.Hspec.Wai

import Backend.App (app)

spec :: Spec
spec = with (return app) $ do
  describe "GET /" $ it "responds with 200" $ get "/" `shouldRespondWith` 200
  describe "GET /flipped"
    $                   it "responds with 200"
    $                   get "/flipped"
    `shouldRespondWith` 200
