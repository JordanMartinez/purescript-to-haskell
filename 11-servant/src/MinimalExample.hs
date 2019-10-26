{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module MinimalExample
    ( startApp
    , webApp
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

{-
These routes were made to look more like Yesod's in the following ways:
- Routes have an 'R' suffix to indicate they are a route
- A route's corresponding handler is defined immediately below it
-}
-- -
type HomeR = Get '[PlainText] String

homeR :: Handler String
homeR = pure "Hello world!"


type FooR = "foo" :> Get '[PlainText] String

fooR :: Handler String
fooR = pure "foo value"


type Routes = HomeR :<|> FooR

server :: Server Routes
server = homeR :<|> fooR


routesProxy :: Proxy Routes
routesProxy = Proxy

webApp :: Application
webApp = serve routesProxy server

startApp :: IO ()
startApp = run 8080 webApp
