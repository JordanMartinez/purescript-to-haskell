{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
module Main (main) where

import Lib (app)
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

main :: IO ()
main = hspec spec

spec :: Spec
spec = with (pure webApp) do
    describe "GET /" do
        it "responds with 200" do
            get "/" `shouldRespondWith` 200
    describe "GET /foo" do
      it "responds with 200" do
        get "/foo" `shouldRespondWith` 200
