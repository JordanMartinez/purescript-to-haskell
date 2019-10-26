{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    , webApp
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

type Route = Get '[PlainText] String

routeHandler :: Handler String
routeHandler = pure "Hello world!"

routeProxy :: Proxy Route
routeProxy = Proxy

server :: Server Route
server = routeHandler

webApp :: Application
webApp = serve routeProxy server

startApp :: IO ()
startApp = run 8080 webApp
