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
import qualified Data.Text as T

-- use the OrgId to link the right data volume
-- and provide right env vars for db connectivity
getCreateContainerR :: OrganisationId -> AccountId -> Handler ()
getCreateContainerR orgid accountid = do
  uid <- requireAuthId
  org <- runDB $ get404 orgid -- should never 404
  account <- runDB $ get404 accountid
  db <- runDB $ get404 $ accountDatabase account
  cport <- liftIO $ getUnassignedPort
  uuid <- liftIO $ nextRandom
  let volumeid = organisationDatavolume org
  let cp = "docker run -t -d --link cirrus-postgres:postgres -e DBNAME=\""++(T.unpack $ databaseDbname db)++"\" -e DBUSER=\""++(T.unpack $ databaseDbuser db)++"\" -e DBPASS=\""++(T.unpack $ databaseDbpassword db)++"\" --volumes-from "++ volumeid ++" -p "++ (show cport)++":8888 notebook start-notebook.sh --NotebookApp.base_url=/notebook/"++(toString uuid)
  $(logInfo) $ T.pack cp
  -- need init to strip trailing newline
  dockerid <- fmap init $ liftIO $ readCreateProcess (shell cp) ""
  now <- liftIO $ getCurrentTime
  maptvar <- fmap appContainerMap getYesod
  liftIO $! atomically $ modifyTVar' maptvar (\m -> Map.insert uuid (ContainerDetails dockerid now uid cport) m)
  _ <- liftIO $ threadDelay 3000000
  $(logInfo) $ "Container " <> (pack dockerid) <> " created"
  redirect $ ProxyR uuid []
