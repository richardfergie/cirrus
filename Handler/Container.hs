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

-- use the OrgId to link the right data volume
-- and provide right env vars for db connectivity
getCreateContainerR :: OrganisationId -> Handler ()
getCreateContainerR orgid = do
  uid <- requireAuthId
  org <- runDB $ get404 orgid -- should never 404
  cport <- liftIO $ getUnassignedPort
  uuid <- liftIO $ nextRandom
  let volumeid = organisationDatavolume org
  let cp = shell $ "docker run -d --volumes-from "++ volumeid ++" -p "++ (show cport)++":8888 jupyter/datascience-notebook start-notebook.sh --NotebookApp.base_url=/notebook/"++(toString uuid)
  -- need init to strip trailing newline
  dockerid <- fmap init $ liftIO $ readCreateProcess cp ""
  now <- liftIO $ getCurrentTime
  maptvar <- fmap appContainerMap getYesod
  liftIO $! atomically $ modifyTVar' maptvar (\m -> Map.insert uuid (ContainerDetails dockerid now uid cport) m)
  _ <- liftIO $ threadDelay 3000000
  $(logInfo) $ "Container " <> (pack dockerid) <> " created"
  redirect $ ProxyR uuid []
