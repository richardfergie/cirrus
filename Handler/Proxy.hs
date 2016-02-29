{-# LANGUAGE ScopedTypeVariables #-}
module Handler.Proxy where

import Import
import Network.HTTP.ReverseProxy
import Data.UUID
import qualified Data.Map.Strict as Map

generateApplication :: Manager -> Int -> Application
generateApplication m p = waiProxyTo (const $ return $ WPRProxyDest $ ProxyDest "localhost" p) defaultOnExc m

proxyRequest :: UUID -> HandlerT App IO ()
proxyRequest uuid = do
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
           app :: Application = generateApplication httpm $ containerPort c
       liftIO $ atomically $ modifyTVar' maptvar (\m -> Map.adjust (\_ -> c') uuid m)
       sendWaiApplication app

getProxyR, postProxyR, putProxyR, deleteProxyR, patchProxyR :: UUID -> Texts -> Handler ()
getProxyR p _ = proxyRequest p

postProxyR p _ = proxyRequest p

putProxyR p _ = proxyRequest p

deleteProxyR p _ = proxyRequest p

patchProxyR p _ = proxyRequest p
