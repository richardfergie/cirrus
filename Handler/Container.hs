{-# LANGUAGE ScopedTypeVariables #-}
module Handler.Container where

import Import
import System.Process
import Data.Streaming.Network (getUnassignedPort)
import Control.Concurrent
import Data.UUID.V4
import Data.UUID
import qualified Data.Map.Strict as Map
import Prelude(init) -- unsafe!

getCreateContainerR :: Handler ()
getCreateContainerR = do
  uid <- requireAuthId
  cport <- liftIO $ getUnassignedPort
  uuid <- liftIO $ nextRandom
  let cp = shell $ "docker run -d -p "++ (show cport)++":8888 jupyter/datascience-notebook start-notebook.sh --NotebookApp.base_url=/proxy/"++(toString uuid)
  -- need init to strip trailing newline
  dockerid <- fmap init $ liftIO $ readCreateProcess cp ""
  now <- liftIO $ getCurrentTime
  maptvar <- fmap appContainerMap getYesod
  liftIO $! atomically $ modifyTVar' maptvar (\m -> Map.insert uuid (ContainerDetails dockerid now uid cport) m)
  _ <- liftIO $ threadDelay 3000000
  $(logInfo) $ "Container " <> (pack dockerid) <> " created"
  redirect $ ProxyR uuid []
