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
import qualified Data.ByteString.Lazy as LBS
import qualified Network.HTTP.Types.Header as HTTP
import Data.Typeable (Typeable)
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Data.List (foldl')

-- ## Type Definitions for Servant

newtype ExceptT_ error monad output
   = ExceptT_ (monad (Either error output))
-- ~           IO (Either error output)

newtype Handler_ output
   = Handler (ExceptT_ ServerError IO output)
-- ~          ExceptT_ (IO (Either ServerError output)
-- ~                    IO (Either ServerError output)

-- Definition of ServerError with 'X' prefixing all of its named entities
-- to prevent name clash in this file
-- Rather than creating this each time, one should use the default values
-- (e.g. `err500`) and override the specific part they need (e.g. add headers,
-- change the reason phrase, add a body, etc.).
data XServerError = XServerError
    { xerrHTTPCode     :: Int
    , xerrReasonPhrase :: String
    , xerrBody         :: LBS.ByteString
    , xerrHeaders      :: [HTTP.Header]
    }
  deriving (Show, Eq, Read, Typeable)

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

singlePathPieceR :: Server SinglePathPieceRoute
singlePathPieceR = pure "single piece"

-- Route acts like a "product type"
-- /piece1/piece2
type MultiPathPieceRoute = "piece1" :> "piece2" :> Get '[PlainText] ReturnType

multiPathPieceR :: Server MultiPathPieceRoute
multiPathPieceR = pure "multi piece"

-- Route acts like a "sum type"
-- /first
-- /second
type TwoPossibleRoutes
  =    "first"  :> Get '[PlainText] ReturnType
  :<|> "second" :> Get '[PlainText] ReturnType

twoPossibleRoutes :: Server TwoPossibleRoutes
twoPossibleRoutes = first :<|> second
  where
    first :: Handler ReturnType
    first = pure "first"

    second :: Handler ReturnType
    second = pure "second"


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

hierarchialRoute :: Server HierarchialRoute
hierarchialRoute = route1 :<|> route2 :<|> routeA :<|> routeB
  where
    route1 :: Handler String
    route1 = pure "route 1"

    route2 :: Handler String
    route2 = pure "route 2"

    routeA :: Handler String
    routeA = pure "route A"

    routeB :: Handler String
    routeB = pure "route B"

-- ### Syntax for Extracting Information from the Path

-- /user/:userId    (e.g. "/user/4" )
type UserIdCaptureRoute = "user" :> Capture "id" Int :> Get '[PlainText] String

userIdCaptureRoute :: Server UserIdCaptureRoute
userIdCaptureRoute = idRoute
  where
    idRoute :: Int -> Handler String
    idRoute a = pure (show a)


   -- /flagQuery       -- "heavy" is false because route doesn't include it
   -- /flagQuery?heavy -- "heavy" is true because route includes it
type SingleFlagQuery
  = "flagQuery" :> QueryFlag "heavy" :> Get '[PlainText] String

singleFlagQuery :: Server SingleFlagQuery
singleFlagQuery = handleRoute
  where
  handleRoute :: Bool -> Handler String
  handleRoute b = pure (show b)


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

multiFlagQuery :: Server MultiFlagQuery
multiFlagQuery = handleFlag
  where
  handleFlag :: Bool -> Bool -> Bool -> Handler String
  handleFlag first second third =
    pure $ show first <>  " " <> show second <> " " <> show third

type QueryParameterValue = String

-- /singleKeyValueQuery?key=value1
type SingleKeyValueQuery
  = "singleKeyValueQuery"
      :> QueryParam "key" QueryParameterValue :> Get '[PlainText] String

singleKeyValueQuery :: Server SingleKeyValueQuery
singleKeyValueQuery = handleRoute
  where
    handleRoute :: Maybe QueryParameterValue -> Handler String
    handleRoute keyValue = pure ("key value was: " <> show keyValue)

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

multiKeyValueQuery :: Server MultiKeyValueQuery
multiKeyValueQuery = handleRoute
  where
    handleRoute
      :: Maybe QueryParameterValue1
      -> Maybe QueryParameterValue2
      -> Maybe QueryParameterValue3
      -> Handler String
    handleRoute value1 value2 value3 = pure (show value1 <> show value2 <> show value3)

data ManyParameterValues
  = MpvValueA
  | MpvValueB
  | MpvValueC
  | MpvValueD
  deriving Show

-- /singleKeyWithMultiValue?key={mpvvaluea, mpvvalueb, mpvvaluec, mpvvalued}
type SingleKeyWithMultiValueQuery
  = "singleKeyWithMultiValue"
    :> QueryParams "key" ManyParameterValues :> Get '[PlainText] String

singleKeyWithMultiValueQuery :: Server SingleKeyWithMultiValueQuery
singleKeyWithMultiValueQuery = handleRoute
  where
    handleRoute :: [ManyParameterValues] -> Handler String
    handleRoute values = pure ("string" <> foldl' (\acc next -> acc <> show next) "" values)

-- ### Syntax for Extracting Information from the Request

-- #### Request Headers

type DecodedRequestHeaderValue = String

type RouteWithRequestHeader
  = "requestHeaders" :> Header "User-Agent" DecodedRequestHeaderValue
                     :> Get '[PlainText] String

routeWithRequestHeader :: Server RouteWithRequestHeader
routeWithRequestHeader = handleRoute
  where
  handleRoute :: Maybe DecodedRequestHeaderValue -> Handler String
  handleRoute decodedHeaderValue = pure (show decodedHeaderValue <> " some stuff")

-- #### Request Body

type DecodedRequestBodyValue = String

type DecodedRequestBody
  = "requestBody" :> ReqBody '[PlainText] DecodedRequestBodyValue
                  :> Get '[PlainText] String

decodedRequestBody :: Server DecodedRequestBody
decodedRequestBody = handleRoute
  where
  handleRoute :: DecodedRequestBodyValue -> Handler String
  handleRoute decodedBody = pure (decodedBody <> " is some body")

-- ### Syntax for Specifying Things in Response

-- #### Response Headers

type ResponseHeaderValue = Int

type SingleResponseHeader
  = "singleResponseHeader"
    :> Get '[PlainText]
        ( Headers '[ Header "My-Header" ResponseHeaderValue ] ReturnType )

singleResponseHeader :: Server SingleResponseHeader
singleResponseHeader = handleRoute
  where
  handleRoute :: Handler (Headers '[ Header "My-Header" ResponseHeaderValue ] ReturnType)
  handleRoute = pure (addHeader 5 "response body")

type MultiResponseHeader
  = "multiResponseHeader"
    :> Get '[PlainText] ( Headers '[ Header "First-Header" ResponseHeaderValue
                                   , Header "Header-Two" ResponseHeaderValue
                                   ]
                         ReturnType
                        )
multiResponseHeader :: Server MultiResponseHeader
multiResponseHeader = handleRoute
  where
  handleRoute :: Handler ( Headers '[ Header "First-Header" ResponseHeaderValue
                                    , Header "Header-Two" ResponseHeaderValue
                                    ]
                          ReturnType
                         )
  handleRoute = pure (addHeader 10 $ addHeader 20 "response body")

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

tryDifferentRouter :: Server TryDifferentRouter
tryDifferentRouter = tryFirst :<|> trySecond :<|> serveStaticFilesViaDirectory
  where
  tryFirst :: Handler String
  tryFirst = pure "first"

  trySecond :: Handler String
  trySecond = pure "second"

  -- Note: this type signature isn't Handler Raw, lest we get a compiler error.
  serveStaticFilesViaDirectory :: Server Raw
  serveStaticFilesViaDirectory = serveDirectoryFileServer "/var/www/"

-- ## Modeling Complex Routes

{-
GET /foo/bar?key=value
POST /foo/bar -- request header has custome header
PUT /foo/bar -- request body stores JSON; response returns JSON
DELETE /foo/bar/:id
GET /path/to/static/file.html
-}

-- Modular Approach
type FooBar remaining =
       ("foo" :> "bar" :> remaining)
  :<|> Raw
type FooBarRoutes
  =    (QueryParam "key" String :> Get '[PlainText] String)
  :<|> (Header "My-Header" Int :> Post '[PlainText] String)
  :<|> (ReqBody '[JSON] Int :> Put '[JSON] Int)
  :<|> (Capture "id" Int :> Delete '[JSON] Int)

type FooBarFullRoute = FooBar FooBarRoutes

{-
-- Same as above

GET /foo/bar?key=value
POST /foo/bar -- request header has custom header
PUT /foo/bar -- request body stores JSON; response returns JSON
DELETE /foo/bar/:id
GET /path/to/static/file.html
-}

-- Full approach
type FooBarFullApproach =
  ("foo" :> "bar" :>
      (  (QueryParam "key" String :> Get '[PlainText] String)
    :<|> (Header "My-Header" Int :> Post '[PlainText] String)
    :<|> (ReqBody '[JSON] Int :> Put '[JSON] Int)
    :<|> (Capture "id" Int :> Delete '[JSON] Int)
      )
  ) :<|> Raw

fooBarFullApproach :: Server FooBarFullApproach
fooBarFullApproach =
       getStringWithKeyParam
  :<|> postStringHeaderInt
  :<|> putJsonIntReqbodyJsonInt
  :<|> deleteIntCaptureInt
  :<|> serveStaticFiles
  where
    getStringWithKeyParam :: Maybe String -> Handler String
    getStringWithKeyParam value = pure (show value <> " and other stuff")

    postStringHeaderInt :: Maybe Int -> Handler String
    postStringHeaderInt intHeader = pure (show intHeader <> " and some other stuff")

    putJsonIntReqbodyJsonInt :: Int -> Handler Int
    putJsonIntReqbodyJsonInt decodedIntValue = pure decodedIntValue

    deleteIntCaptureInt :: Int -> Handler Int
    deleteIntCaptureInt capturedIntValue = pure capturedIntValue

    serveStaticFiles :: Server Raw -- Server, not Handler, for Raw
    serveStaticFiles = serveStaticFilesViaDirectory "/var/www/"

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
    :> Header "Request-Header" DecodedRequestHeaderValue
    :> ReqBody '[PlainText] DecodedRequestBodyValue
    :> Get '[PlainText]
        (Headers '[ Header "Response-Header" ResponseHeaderValue
                  ]
         ReturnType)

routeOrderAndPossibleSyntax :: Server RouteOrderAndPossibleSyntax
routeOrderAndPossibleSyntax = handleRoute
  where
    handleRoute
      :: Int -> Maybe Int -> Bool -> Maybe DecodedRequestHeaderValue -> DecodedRequestBodyValue
      -> Handler (Headers '[ Header "Response-Header" ResponseHeaderValue
                           ]
                  ReturnType)
    handleRoute capturedInt maybeParamValue heavyFlag reqHeader reqBody =
      pure (addHeader 5 "request body")

-- ## Using Template Haskell to derive JSON instances for a data type

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)
