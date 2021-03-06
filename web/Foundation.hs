{-# OPTIONS_GHC -fno-warn-orphans #-}
module Foundation where

import Import.NoFoundation
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Text.Hamlet          (hamletFile)
import Text.Jasmine         (minifym)
import Yesod.Auth.HashDB (HashDBUser(..), authHashDB, getAuthIdHashDB)
import Yesod.Default.Util   (addStaticContentExternal)
import Yesod.Core.Types     (Logger)
import qualified Yesod.Core.Unsafe as Unsafe
import qualified Data.CaseInsensitive as CI
import qualified Data.Text.Encoding as TE
import qualified Data.Map.Strict as Map
import Data.UUID
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
    , appContainerMap :: TVar (Map.Map UUID ContainerDetails)
    , appDockerIPAddr :: String
    }

data ContainerDetails = ContainerDetails { dockerId :: String,
                                           lastAction :: UTCTime,
                                           containerUser :: UserId,
                                           containerPort :: Int
                                         }

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
-- type Handler = HandlerT App IO
-- type Widget = WidgetT App IO ()
mkYesodData "App" $(parseRoutesFile "config/routes")

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot = ApprootRequest $ \app req ->
        case appRoot $ appSettings app of
            Nothing -> getApprootText guessApproot app req
            Just root -> root

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = Just <$> defaultClientSessionBackend
        120    -- timeout in minutes
        "config/client_session_key.aes"

    -- Yesod Middleware allows you to run code before and after each handler function.
    -- The defaultYesodMiddleware adds the response header "Vary: Accept, Accept-Language" and performs authorization checks.
    -- The defaultCsrfMiddleware:
    --   a) Sets a cookie with a CSRF token in it.
    --   b) Validates that incoming write requests include that token in either a header or POST parameter.
    -- For details, see the CSRF documentation in the Yesod.Core.Handler module of the yesod-core package.
    yesodMiddleware r = do
              croute <- getCurrentRoute
              case croute of
                Just (ProxyR _ _) -> r
                _ -> defaultYesodMiddleware r

    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            addStylesheet $ StaticR css_bootstrap_css
            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    -- Routes not requiring authentication.
    isAuthorized (AuthR _) _ = return Authorized
    isAuthorized FaviconR _ = return Authorized
    isAuthorized RobotsR _ = return Authorized
    isAuthorized UserR _ = do
      mu <- maybeAuthId
      case mu of
       Nothing -> return AuthenticationRequired
       Just _ -> return Authorized
    isAuthorized (ProxyR uuid _) _ = do
      uid <- requireAuthId
      maptvar <- fmap appContainerMap getYesod
      containers <- liftIO $ atomically $ readTVar maptvar
      case Map.lookup uuid containers of
        Nothing -> return $ Unauthorized "Unknown container"
        Just (ContainerDetails _ _ cuid _) ->
          if cuid == uid
             then return Authorized
             else return $ Unauthorized "Unknown container"
    isAuthorized CreateOrganisationR _ = do
      mu <- maybeAuthId
      case mu of
       Nothing -> return AuthenticationRequired
       Just _ -> return Authorized
    isAuthorized (OrganisationR oid) _ = organisationUserCheck oid
    isAuthorized (CreateContainerR oid accountid) _ = do
      ou <- organisationUserCheck oid
      case ou of
        Authorized -> organisationAccountCheck oid accountid
        _ -> organisationUserCheck oid
    -- Default to Authorized for now.
    isAuthorized (CreateDatabaseR oid) _ = organisationUserCheck oid
    isAuthorized (CreateAccountR oid) _ = organisationUserCheck oid
    isAuthorized _ _ = return Authorized

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
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

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog app _source level =
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger = return . appLogger

organisationUserCheck :: OrganisationId -> Handler AuthResult
organisationUserCheck orgid = do
  mu <- maybeAuthId
  case mu of
   Nothing -> return AuthenticationRequired
   Just aid -> do
     x <- runDB $ getBy $ UniqueOrganisationUser orgid aid
     case x of
      Nothing -> return $ Unauthorized "You are not part of this org"
      Just _ -> return Authorized

organisationAccountCheck :: OrganisationId -> AccountId -> Handler AuthResult
organisationAccountCheck orgid accid = do
  macc <- runDB $ get accid
  case macc of
   Nothing -> return $ Unauthorized "You cannot access this account"
   Just (Account oid _ _ _ _) -> case orgid==oid of
     False -> return $ Unauthorized "You cannot access this account"
     True -> return $ Authorized

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest _ = UserR
    -- Where to send a user after logout
    logoutDest _ = HomeR
    -- Override the above two destinations when a Referer: header is present
    redirectToReferer _ = True

    -- You can add other plugins like BrowserID, email or OAuth here
    authPlugins _ = [authHashDB (Just . UniqueUser)]

    getAuthId creds = getAuthIdHashDB AuthR (Just . UniqueUser) creds
    authHttpManager = error "Email doesn't need an HTTP manager"

instance HashDBUser User where
    userPasswordHash = userPassword
    setPasswordHash h u = u { userPassword = Just h }

instance YesodAuthPersist App

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- Useful when writing code that is re-usable outside of the Handler context.
-- An example is background jobs that send email.
-- This can also be useful for writing code that works across multiple Yesod applications.
instance HasHttpManager App where
    getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

instance PathPiece UUID where
  fromPathPiece = fromText
  toPathPiece = toText


-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding

