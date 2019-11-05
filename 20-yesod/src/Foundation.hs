{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Foundation where

import Import.NoFoundation
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Text.Jasmine         (minifym)
import Control.Monad.Logger (LogSource)

-- Used only when in "auth-dummy-login" setting is enabled.
import Yesod.Auth.Dummy

import Yesod.Auth.OpenId    (authOpenId, IdentifierType (Claimed))
import Yesod.Default.Util   (addStaticContentExternal)
import Yesod.Core.Types     (Logger)
import qualified Yesod.Core.Unsafe as Unsafe
-- import qualified Data.CaseInsensitive as CI
-- import qualified Data.Text.Encoding as TE

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    }

data MenuItem = MenuItem
    { menuItemLabel :: Text
    , menuItemRoute :: Route App
    , menuItemAccessCallback :: Bool
    }

data MenuTypes
    = NavbarLeft MenuItem
    | NavbarRight MenuItem

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the following documentation
-- for an explanation for this split:
-- http://www.yesodweb.com/book/scaffolding-and-the-site-template#scaffolding-and-the-site-template_foundation_and_application_modules
--
-- This function also generates the following type synonyms:
--    (deprecated! see comment below) type Handler = HandlerT App IO
--    (deprecated! see commnt below)  type Widget = WidgetT App IO ()
mkYesodData "App" $(parseRoutesFile "config/routes")

-- Handler/HandlerT is deprecated:
-- https://hackage.haskell.org/package/yesod-core-1.6.16.1/docs/Yesod-Core-Handler.html#t:HandlerT
-- Widget/WidgetT is deprecated∷
-- https://hackage.haskell.org/package/yesod-core-1.6.16.1/docs/Yesod-Core-Widget.html#t:WidgetT
type HandlerX = HandlerFor App
type WidgetX = WidgetFor App

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerFor App) (FormResult x, Widget)

-- | A convenient synonym for database access functions.
type DB a = forall (m :: * -> *).
    (MonadIO m) => ReaderT SqlBackend m a

-- | Timeout in minutes. Used in `defaultClientSessionBackend`.
clientSessionTimeout :: Int
clientSessionTimeout = 120 -- minutes

-- | Timeout in minutes. Used in `sslOnlyMiddleware`. Ensures that
-- HTTP requests will not be made for as long as (if not longer than)
-- the client session cookie's timeout. To increase the timeout, change `0` to
-- a positive value.
strictTransportSecurityTimeout :: Int
strictTransportSecurityTimeout =
  clientSessionTimeout + 0 -- minutes

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- === Application-related things ===

    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot :: Approot App
    approot = ApprootRequest $ \app req ->
        case appRoot $ appSettings app of
            Nothing -> getApprootText guessApproot app req
            Just root -> root

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    -- Also ensure that these sessions are only transmitted via HTTPS
    makeSessionBackend :: App -> IO (Maybe SessionBackend)
    makeSessionBackend _ = sslOnlySessions $ fmap Just $ defaultClientSessionBackend
        clientSessionTimeout
        "config/client_session_key.aes"

    -- Yesod Middleware allows you to run code before and after each handler function.
    -- The defaultYesodMiddleware adds the response header "Vary: Accept, Accept-Language" and performs authorization checks.
    -- Some users may also want to add the defaultCsrfMiddleware, which:
    --   a) Sets a cookie with a CSRF token in it.
    --   b) Validates that incoming write requests include that token in either a header or POST parameter.
    -- To add it, chain it together with the defaultMiddleware: yesodMiddleware = defaultYesodMiddleware . defaultCsrfMiddleware
    -- For details, see the CSRF documentation in the Yesod.Core.Handler module of the yesod-core package.
    -- Also add `sslOnlyMiddleware` with same client session timeout as
    yesodMiddleware :: ToTypedContent res => Handler res -> Handler res
    yesodMiddleware =
      (sslOnlyMiddleware strictTransportSecurityTimeout) .
        defaultYesodMiddleware . defaultCsrfMiddleware

    -- === Path-related things ===

    --cleanPath :: site -> [Text] -> Either [Text] [Text]
    --cleanPath = default implementation should be fine

    --joinPath :: site -> T.Text -> [T.Text] -> [(T.Text, T.Text)] -> Builder
    --joinPath = default implementation should be fine

    -- === Route-related things ===

    -- The page to be redirected to when authentication is required.
    authRoute
        :: App
        -> Maybe (Route App)
    authRoute _ = Just $ AuthR LoginR

    isAuthorized
        :: Route App  -- ^ The route the user is visiting.
        -> Bool       -- ^ Whether or not this is a "write" request.
        -> Handler AuthResult
    -- Routes requiring authorization
    isAuthorized AuthorizedByPathR _ = pure (Unauthorized "Requires authorization")
    isAuthorized route@(AuthByTLAttribR _) _ =
      if "requiresAuthorization" `member` routeAttrs route
      then do
        muser <- maybeAuthId
        case muser of
          Nothing -> pure AuthenticationRequired
          Just _ -> pure Authorized
      else pure (Unauthorized "Needs authentication")

    -- All other routes don't require authorization.
    isAuthorized _ _ = pure Authorized

    -- === Miscellaneous Application things ===

    -- maximumContentLength :: App -> Maybe (Route site) -> Maybe Word64
    -- maximumContentLength _ _ = Just $ 2 * 1024 * 1024 -- 2 megabytes

    -- fileUpload :: App -> RequestBodyLength -> FileUpload
    -- fileUpload _ _
    --   -- default shown below
    --   | when req body > 50kb || chunked req body = store in temp file
    --   | otherwise = store in memory

    -- === Logger-related things ===

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLogIO :: App -> LogSource -> LogLevel -> IO Bool
    shouldLogIO app _source level =
        return $
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger :: App -> IO Logger
    makeLogger = return . appLogger

    -- === Server-Side Rendering-related things ===

    -- defaultLayout :: Widget -> Handler Html
    -- defaultLayout {- widget -} = do
        -- master <- getYesod
        -- mmsg <- getMessage

        -- muser <- maybeAuthPair
        -- mcurrentRoute <- getCurrentRoute

        -- Get the breadcrumbs, as defined in the YesodBreadcrumbs instance.
        -- (title, parents) <- breadcrumbs

        -- pure $ toHtml "foo"

    --errorHandler :: ErrorResponse -> HandlerFor App TypedContent
    --errorHandler = defaultErrorHandler

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent
        :: Text  -- ^ The file extension
        -> Text -- ^ The MIME content type
        -> LByteString -- ^ The contents of the file
        -> Handler (Maybe (Either Text (Route App, [(Text, Text)])))
    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = appStaticDir $ appSettings master
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" ++ base64md5 lbs

-- Define breadcrumbs.
-- instance YesodBreadcrumbs App where
    -- Takes the route that the user is currently on, and returns a tuple
    -- of the 'Text' that you want the label to display, and a previous
    -- breadcrumb route.
    -- breadcrumb
        -- :: Route App  -- ^ The route the user is visiting currently.
        -- -> Handler (Text, Maybe (Route App))
    -- breadcrumb HomeR = return ("Home", Nothing)
    -- breadcrumb (AuthR _) = return ("Login", Just HomeR)
    -- breadcrumb ProfileR = return ("Profile", Just HomeR)
    -- breadcrumb  _ = return ("home", Nothing)

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB :: SqlPersistT (HandlerFor App) a -> HandlerFor App a
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master

instance YesodPersistRunner App where
    getDBRunner :: HandlerFor App (DBRunner App, Handler ())
    getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest :: App -> Route App
    loginDest _ = HomeR
    -- Where to send a user after logout
    logoutDest :: App -> Route App
    logoutDest _ = HomeR
    -- Override the above two destinations when a Referer: header is present
    redirectToReferer :: App -> Bool
    redirectToReferer _ = True

    authenticate :: (MonadHandler m, HandlerSite m ~ App)
                 => Creds App -> m (AuthenticationResult App)
    authenticate creds = liftHandler $ runDB $ do
        x <- getBy $ UniqueUser $ credsIdent creds
        case x of
            Just (Entity uid _) -> return $ Authenticated uid
            Nothing -> Authenticated <$> insert User
                { userIdent = credsIdent creds
                , userPassword = Nothing
                }

    -- You can add other plugins like Google Email, email or OAuth here
    authPlugins :: App -> [AuthPlugin App]
    authPlugins app = [authOpenId Claimed []] ++ extraAuthPlugins
        -- Enable authDummy login if enabled.
        where extraAuthPlugins = [authDummy | appAuthDummyLogin $ appSettings app]

-- | Access function to determine if a user is logged in.
isAuthenticated :: HandlerFor App AuthResult
isAuthenticated = do
    muid <- maybeAuthId
    return $ case muid of
        Nothing -> Unauthorized "You must login to access this page"
        Just _ -> Authorized

instance YesodAuthPersist App

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage :: App -> [Lang] -> FormMessage -> Text
    renderMessage _ _ = defaultFormMessage

-- Useful when writing code that is re-usable outside of the Handler context.
-- An example is background jobs that send email.
-- This can also be useful for writing code that works across multiple Yesod applications.
instance HasHttpManager App where
    getHttpManager :: App -> Manager
    getHttpManager = appHttpManager

unsafeHandler :: App -> HandlerFor App a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding
