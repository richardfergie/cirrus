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

gp :: RequestHeaders -> IO (Either a ProxyDest)
gp = error "Running gp"

makeProxy :: ProxyDest -> Handler ()
makeProxy destination = liftIO $ rawProxyTo gp $ error "Skipping gp"

bar :: Manager -> Int -> Application
bar m p = waiProxyTo (const $ return $ WPRProxyDest $ ProxyDest "localhost" p) defaultOnExc m

foo port = do
  m <- fmap appHttpManager getYesod
  let app :: Application = bar m port
  sendWaiApplication app

getCreateThingyR :: Handler ()
getCreateThingyR = do
  port <- liftIO $ getUnassignedPort
  _ <- liftIO $ spawnCommand $ "docker run -p "++ (show port)++":8888 jupyter/datascience-notebook start-notebook.sh --NotebookApp.base_url=/proxy/"++(show port)
  _ <- liftIO $ threadDelay 3000000
  redirect $ ProxyR port []

getProxyR, postProxyR, putProxyR, deleteProxyR, patchProxyR :: Int -> Texts -> Handler ()
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

