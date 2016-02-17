{-# LANGUAGE ScopedTypeVariables #-}
module Handler.Home where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput)
import Text.Julius (RawJS (..))
import Network.HTTP.ReverseProxy
import System.Process
import Data.Streaming.Network (getUnassignedPort)
import Control.Concurrent
import Data.UUID.V4
import Data.UUID
import qualified Data.Map.Strict as Map
-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
    (formWidget, formEnctype) <- generateFormPost sampleForm
    let submission = Nothing :: Maybe (FileInfo, Text)
        handlerName = "getHomeR" :: Text
    defaultLayout $ do
        let (commentFormId, commentTextareaId, commentListId) = commentIds
        aDomId <- newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

postHomeR :: Handler Html
postHomeR = do
    ((result, formWidget), formEnctype) <- runFormPost sampleForm
    let handlerName = "postHomeR" :: Text
        submission = case result of
            FormSuccess res -> Just res
            _ -> Nothing

    defaultLayout $ do
        let (commentFormId, commentTextareaId, commentListId) = commentIds
        aDomId <- newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

bar :: Manager -> Int -> Application
bar m p = waiProxyTo (const $ return $ WPRProxyDest $ ProxyDest "localhost" p) defaultOnExc m

foo uuid = do
  uid <- requireAuthId
  httpm <- fmap appHttpManager getYesod
  maptvar <- fmap appContainerMap getYesod
  now <- liftIO $ getCurrentTime
  containers <- liftIO $ atomically $ readTVar maptvar
  case Map.lookup uuid containers of
   Nothing -> error "Cannot find uuid in map"
   Just c -> case (containerUser c == uid) of
     False -> error "User cannot access this container"
     True -> do
       let c' = c{ lastAction = now}
           app :: Application = bar httpm $ containerPort c
       liftIO $ atomically $ modifyTVar' maptvar (\m -> Map.adjust (\_ -> c') uuid m)
       sendWaiApplication app

getCreateThingyR :: Handler ()
getCreateThingyR = do
  uid <- requireAuthId
  cport <- liftIO $ getUnassignedPort
  uuid <- liftIO $ nextRandom
  phandle <- liftIO $ spawnCommand $ "docker run -p "++ (show cport)++":8888 jupyter/datascience-notebook start-notebook.sh --NotebookApp.base_url=/proxy/"++(toString uuid)
  now <- liftIO $ getCurrentTime
  maptvar <- fmap appContainerMap getYesod
  liftIO $ atomically $ modifyTVar' maptvar (\m -> Map.insert uuid (ContainerDetails phandle now uid cport) m)
  _ <- liftIO $ threadDelay 3000000
  redirect $ ProxyR uuid []

getProxyR, postProxyR, putProxyR, deleteProxyR, patchProxyR :: UUID -> Texts -> Handler ()
getProxyR p _ = foo p

postProxyR p _ = foo p

putProxyR p _ = foo p

deleteProxyR p _ = foo p

patchProxyR p _ = foo p

sampleForm :: Form (FileInfo, Text)
sampleForm = renderBootstrap3 BootstrapBasicForm $ (,)
    <$> fileAFormReq "Choose a file"
    <*> areq textField (withSmallInput "What's on the file?") Nothing

commentIds :: (Text, Text, Text)
commentIds = ("js-commentForm", "js-createCommentTextarea", "js-commentList")

