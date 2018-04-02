{-# LANGUAGE OverloadedStrings #-}

module Backend.AppSpec where

import Backend.App (app)
import Test.Hspec
import Test.Hspec.Wai

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  with (return app) $ do
    describe "GET /" $ do
      it "responds with 200" $ get "/" `shouldRespondWith` 200
      it "responds with 'Simple'" $ get "/" `shouldRespondWith` "Simple"
    describe "GET /flipped" $
      it "responds with 200" $ get "/flipped" `shouldRespondWith` 200
