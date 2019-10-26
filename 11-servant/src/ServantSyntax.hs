{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module ServantSyntax
    (
    -- startApp
    -- app
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

-- data User = User
--   { userId        :: Int
--   , userFirstName :: String
--   , userLastName  :: String
--   } deriving (Eq, Show)
--
-- $(deriveJSON defaultOptions ''User)

-- ## Servant's Type-Level Route Syntax

-- ### Syntax for Specifying HTTP Method Used in Request

-- #### Returning a Single Value

-- The underscore is necessary here to prevent naming clashes with
-- real type aliases used (i.e. read "Get_" and "Get")
-- type Method_ = Verb Method ReturnedStatusCode
type Get_ = Verb 'GET 200
type Post_ = Verb 'POST 200
type Put_ = Verb 'PUT 200
type Delete_ = Verb 'DELETE 200

type PostCreated = Verb 'POST 201
type PostAccepted = Verb 'POST 202

-- #### Returning a Stream of Values

type StreamGet_ = Stream 'GET 200
-- and same for other HTTP methods

type ReturnType' = String

-- 3 standard framing strategies

type ContentType = PlainText

type StreamingRouteNewlineFraming
  = StreamGet NewlineFraming ContentType (SourceIO ReturnType')

type StreamingRouteNetstringFraming
  = StreamGet NetstringFraming ContentType (SourceIO ReturnType')

type StreamingRouteNoFraming
  = StreamGet NoFraming ContentType (SourceIO ReturnType')

-- ### Route Syntax

type ReturnType = String

-- GET request on the `/` path which returns a `ReturnType` in 4
--    different ways (i.e. Content-Type header):
--    PlainText, JSON, FormUrlEncoded, or an OctetStream
type RootRoute = Get '[PlainText, JSON, FormUrlEncoded, OctetStream] ReturnType

-- GET request on the `/` path which returns a stream of
--    `ReturnType` values as PlainText.
type RootRoute' = StreamGet NoFraming PlainText ReturnType

rootRouteServer :: Server RootRoute
rootRouteServer = pure "home"

-- /singlePiece
type SinglePathPieceRoute = "singlePiece" :> Get '[PlainText] ReturnType

-- Route acts like a "product type"
-- /piece1/piece2
type MultiPathPieceRoute = "piece1" :> "piece2" :> Get '[PlainText] ReturnType

-- Route acts like a "sum type"
-- /first
-- /second
type TwoPossibleRoutes
  =    "first"  :> Get '[PlainText] ReturnType
  :<|> "second" :> Get '[PlainText] ReturnType


-- Route acts like a tree: same parent piece with different children pieces
-- /hierarchy/route1
-- /hierarchy/route2
-- /hierarchy/subhierarchy/routeA
-- /hierarchy/subhierarchy/routeA
type HierarchialRoute
  = "hierarchy" :>
    (  "route1" :> Get '[PlainText] String
  :<|> "route2" :> Get '[PlainText] String
  :<|> "subhierarchy" :>
       (  "routeA" :> Get '[PlainText] String
     :<|> "routeB" :> Get '[PlainText] String
       )
   )

-- ### Syntax for Extracting Information from the Path

-- /user/:userId    (e.g. "/user/4" )
type UserIdCaptureRoute = "user" :> Capture "id" Int :> Get '[PlainText] String

   -- /flagQuery       -- "heavy" is false because route doesn't include it
   -- /flagQuery?heavy -- "heavy" is true because route includes it
type SingleFlagQuery
  = "flagQuery" :> QueryFlag "heavy" :> Get '[PlainText] String

-- Various combinations
-- /flagQuery?first=&second=&third=
-- /flagQuery?first=&third=
-- /flagQuery?second=&first=&third=
type MultiFlagQuery
  = "flagQuery"
      :> QueryFlag "first"
      :> QueryFlag "second"
      :> QueryFlag "third"
      :> Get '[PlainText] String

type QueryParameterValue = String

-- /singleKeyValueQuery?key=value1
type SingleKeyValueQuery
  = "singleKeyValueQuery"
      :> QueryParam "key" QueryParameterValue :> Get '[PlainText] String

type QueryParameterValue1 = String
type QueryParameterValue2 = String
type QueryParameterValue3 = String

-- /multiKeyValueQuery?key1=value1&key2=value2&key3=value3
type MultiKeyValueQuery
  = "multiKeyValueQuery"
      :> QueryParam "key1" QueryParameterValue1
      :> QueryParam "key2" QueryParameterValue2
      :> QueryParam "key3" QueryParameterValue3
      :> Get '[PlainText] String

data ManyParameterValues
  = MpvValueA
  | MpvValueB
  | MpvValueC
  | MpvValueD

-- /singleKeyWithMultiValue?key={mpvvaluea, mpvvalueb, mpvvaluec, mpvvalued}
type SingleKeyWithMultiValueQuery
  = "singleKeyWithMultiValue"
    :> QueryParams "key" QueryParameterValue :> Get '[PlainText] String

-- ### Syntax for Extracting Information from the Request

-- #### Request Headers

type DecodedHeaderValue = String

type RouteWithHeader
  = "requestHeaders" :> Header "User-Agent" DecodedHeaderValue
                     :> Get '[PlainText] String

-- #### Request Body

type DecodedBodyValue = String

type DecodedBody
  = "requestBody" :> ReqBody '[PlainText] DecodedBodyValue
                  :> Get '[PlainText] String

-- ### Syntax for Specifying Things in Response

-- #### Response Headers

type EncodedHeaderValue = Int

type SingleResponseHeader
  = "singleResponseHeader"
    :> Get '[PlainText]
        ( Headers '[ Header "My-Header" EncodedHeaderValue ] ReturnType )

type MultiResponseHeader
  = "multiResponseHeader"
    :> Get '[PlainText] ( Headers '[ Header "First-Header" EncodedHeaderValue
                                   , Header "Header-Two" EncodedHeaderValue
                                   ]
                         ReturnType
                        )

-- ### Creating Prefixes to Routes

type PrefixedRoute innerRoute = "prefix" :> innerRoute
type RemainingRoute = "inner" :> Get '[PlainText] String

-- /prefix/inner
type FullRoute = PrefixedRoute RemainingRoute

-- /prefix/prefix
type NoMoreRoutes = PrefixedRoute (PrefixedRoute EmptyAPI)

-- ### Catch-All Match

type TryDifferentRouter =
       "tryMeFirst" :> Get '[PlainText] String
  :<|> "tryMeSecond" :> Get '[PlainText] String
  :<|> Raw -- indicates anything that hasn't matched thus far
           -- should be handled as a WAI Application
           -- (e.g. another web application, static file server, etc.)

-- ## Modeling Complex Routes

{-
GET /foo/bar?key=value
POST /foo/bar -- request header has custome header
PUT /foo/bar -- request body stores JSON; response returns JSON
DELETE /foo/bar/:id
-}

-- Modular Approach
type FooBar remaining = "foo" :> "bar" :> remaining
type FooBarRoutes
  =    (QueryParam "key" String :> Get '[PlainText] String)
  :<|> (Header "My-Header" Int :> Post '[PlainText] String)
  :<|> (ReqBody '[JSON] Int :> Put '[JSON] Int)
  :<|> (Capture "id" Int :> Delete '[JSON] Int)

type FooBarFullRoute = FooBar FooBarRoutes

{-
-- Same as above

GET /foo/bar?key=value
POST /foo/bar -- request header has custome header
PUT /foo/bar -- request body stores JSON; response returns JSON
DELETE /foo/bar/:id
-}

-- Full approach
type FooBarFullApproach =
  "foo" :> "bar" :>
      (  (QueryParam "key" String :> Get '[PlainText] String)
    :<|> (Header "My-Header" Int :> Post '[PlainText] String)
    :<|> (ReqBody '[JSON] Int :> Put '[JSON] Int)
    :<|> (Capture "id" Int :> Delete '[JSON] Int)
      )

-- ## Full Syntax

-- GET http://example.com/staticPathPiece/:pieceName?key=4&heavy
-- Request-Header: headerValue
--
-- ========== Server ==========
--
-- Content-Type: text/plain
-- Response-Header: responseHeaderValue
--
-- value of 'ReturnType' encoded as plain text
type RouteOrderAndPossibleSyntax =
  "staticPathPiece"
    :> Capture "pieceName" Int
    :> QueryParam "key" Int
    :> QueryFlag "heavy"
    :> Header "Request-Header" DecodedHeaderValue
    :> ReqBody '[PlainText] DecodedBodyValue
    :> Get '[PlainText]
        (Headers '[ Header "Response-Header" EncodedHeaderValue
                  ]
         ReturnType)

{-
startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve routesProxy server

routesProxy :: Proxy Routes
routesProxy = Proxy

server :: Server Routes
server = pure "foo"

-}
