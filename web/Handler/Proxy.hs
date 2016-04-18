{-# LANGUAGE ScopedTypeVariables #-}
module Handler.Proxy where

import Import
import Network.HTTP.ReverseProxy
import Data.UUID
import qualified Data.Map.Strict as Map
import qualified Data.ByteString.Char8 as C

generateApplication :: Manager -> String -> Int -> Application
generateApplication m ip p = waiProxyTo (const $ return $ WPRProxyDest $ ProxyDest (C.pack ip) p) defaultOnExc m

proxyRequest :: UUID -> HandlerT App IO ()
proxyRequest uuid = do
  uid <- requireAuthId
  httpm <- fmap appHttpManager getYesod
  maptvar <- fmap appContainerMap getYesod
  now <- liftIO $ getCurrentTime
  containers <- liftIO $ atomically $ readTVar maptvar
  ipaddr <- fmap appDockerIPAddr getYesod
  case Map.lookup uuid containers of
   Nothing -> error "Cannot find uuid in map"
   Just c -> case (containerUser c == uid) of
     False -> error "User cannot access this container"
     True -> do
       let c' = c{ lastAction = now}
           app :: Application = generateApplication httpm ipaddr $ containerPort c
       liftIO $ atomically $ modifyTVar' maptvar (\m -> Map.adjust (\_ -> c') uuid m)
       sendWaiApplication app

getProxyR, postProxyR, putProxyR, deleteProxyR, patchProxyR :: UUID -> Texts -> Handler ()
getProxyR p _ = proxyRequest p

postProxyR p _ = proxyRequest p

putProxyR p _ = proxyRequest p

deleteProxyR p _ = proxyRequest p

patchProxyR p _ = proxyRequest p
