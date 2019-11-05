{-# LANGUAGE NoImplicitPrelude #-}
module Handler.Routes where

import Import
import qualified Data.CaseInsensitive as CI
import Data.CaseInsensitive (CI(..))
import Web.Cookie (setCookieName, setCookiePath, setCookieExpires, setCookieValue, setCookieMaxAge, setCookieDomain, setCookieSecure, setCookieHttpOnly, setCookieSameSite, defaultSetCookie, sameSiteStrict)

-- ## Preface
--
-- Normally, we'd use Handler here. However, it's deprecated.
-- https://hackage.haskell.org/package/yesod-core-1.6.16.1/docs/Yesod-Core-Handler.html#t:HandlerT
--
-- so, we've defined our own type alias in `Foundation.hs` to replace it
-- called `HandlerX` and we'll be using it here below.
-- type HandlerX a = HandleFor App a

-- ## Route Examples

-- ### No Authorization needed

getHomeR :: HandlerX String
getHomeR = pure "home - get"

postHomeR :: HandlerX String
postHomeR = pure "home - post"

handleAllHttpMethodsR :: HandlerX String
handleAllHttpMethodsR = pure "all http methods handled regardless of method"

getPathR :: HandlerX String
getPathR = pure "path"

getPathPieceR :: Int -> HandlerX String
getPathPieceR i = pure ("path piece " <> show i)

-- getMultiPathPieceR :: ThreeInts -> HandlerX String
-- getMultiPathPieceR (ThreeInts (a, b, c)) = pure ("path pieces: " <> show (a + b + c))

-- getMultipleIntArgsR :: [Int] -> HandlerX String
-- getMultipleIntArgsR list = pure (foldl' (\acc next -> acc <> show next) "" list)

getFirstR :: HandlerX String
getFirstR = pure "first"

getAttributeR :: HandlerX String
getAttributeR = pure "attribute route"

getAfterFirstR :: Int -> HandlerX String
getAfterFirstR i = pure ("after first: " <> show i)

getRoute1R :: HandlerX String
getRoute1R = pure "route 1 r"

getRoute2R :: HandlerX String
getRoute2R = pure "route 2 r"

getFinalR :: HandlerX String
getFinalR = pure "Final R"

getInnerRouteR :: HandlerX String
getInnerRouteR = pure "inner route"

getSubRouteR :: HandlerX String
getSubRouteR = pure "sub route"

getEndRouteR :: HandlerX String
getEndRouteR = pure "end route"

-- See `config/routes` file for corresponding section.
-- Invalid syntax: a route cannot respond to a request (as indicated
-- by 'GET') and be a hierarchical route at the same time (as indicated
-- by the ':').
--
-- Uncomment these lines to see for yourself.
--getHierarchyAndHttpMethod :: HandlerX String
--getHierarchyAndHttpMethod = pure "hierarchy and http method"
--
--getInnerValRouteR :: HandlerX String
--getInnerValRouteR = pure "inner val route"

-- ### Authorization needed

getAuthorizedByPathR :: HandlerX String
getAuthorizedByPathR = pure "authorized by path"

getAuthedFirstR :: HandlerX String
getAuthedFirstR = pure "first - authorized by top-level attribute"

getAuthedSecondR :: HandlerX String
getAuthedSecondR = pure "second - authorized by top-level attribute"

-- ### Handler Functions

getHandlerFunctionsR :: HandlerX String
getHandlerFunctionsR = do
  yesod <- getYesod
  yesod2 <- ask
  configValue <- asks \yesod3 -> {- getValue -} yesod3

-- #### Query Parameters

  maybeParameterValue <- lookupGetParam "queryParam"
  maybeListParameterValues <- lookupGetParams "queryParam"

-- #### Headers

  -- any header
  addHeader "some-header" "value"
  replaceOrAddHeader "some-header-two" "value"

  -- these won't work until next request
  let someHeader = "some-header" :: CI ByteString
  maybeHeaderValue <- lookupHeader someHeader
  maybeListHeaderValues <- lookupHeaders someHeader

  -- Sets the language for user's session
  setLanguage "en-US"

  -- List can include values in this order:
  -- user session, then parameter, then cookie, then "Accept-Language" header
  listOfLanguages <- languages

-- ##### Specific Headers

  -- Content-Disposition: attachment; filename="file-name.jpg"
  addContentDispositionFileName "file-name.jpg"

  -- Cache-Control: max-age=20, public
  cacheSeconds 20

  -- Expires: HTTP-date
  nowUtcTime <- liftIO getCurrentTime
  neverExpires -- some date in 2037
  alreadyExpired -- date in past, so don't cache it
  expiresAt nowUtcTime

-- #### Cookies

  setCsrfCookie

  twoHourExpiration <- getExpires 120 -- minutes
  let myCookie =
        defaultSetCookie { setCookieName = "cookie-name"
                         , setCookieValue = "cookie-value"
                         , setCookiePath     = Just "/"
                         , setCookieExpires  = Just twoHourExpiration
                         , setCookieMaxAge   = Nothing
                         , setCookieDomain   = Nothing
                         , setCookieHttpOnly = True
                         , setCookieSecure   = True
                         , setCookieSameSite = Just sameSiteStrict
                         }

  setCookie myCookie

  -- these won't work until next request
  maybeCookieValue <- lookupCookie "cookie-name"
  maybeListCookieValues <- lookupCookies "some-cookie"

  deleteCookie "key" "path"

-- #### Sessions

  sessionMap <- getSession

  maybeTextValue <- lookupSession "key"
  maybeByteStringValue <- lookupSessionBS "key"

  setSession "key" "text value"
  setSessionBS "key" "bytestring value"

  deleteSession "key"

  clearSession

  -- post-redirect-get pattern
  setMessage "some message value"
  someMessageValue <- getMessage

  pure "get handler functions"

postHandlerFunctionsR :: HandlerX String
postHandlerFunctionsR = do
  maybeParameterValue <- lookupPostParam "param"
  maybeListParameterValues <- lookupPostParams "param"

  pure "handler function - post"
