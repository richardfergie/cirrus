module Handler.User where

import Import
import System.Process
import Prelude(init) --unsafe!!
import Data.UUID.V4

getUserR :: Handler Html
getUserR = do
  aid <- requireAuthId
  orgs <- runDB $ selectList [OrganisationUserUser ==. aid] []
  case orgs of
   [] -> redirect CreateOrganisationR
   (Entity _ (OrganisationUser orgid _)):[] -> redirect $ OrganisationR orgid
   _ -> error "Many organisations"

getCreateOrganisationR :: Handler Html
getCreateOrganisationR = do
  (orgform, enctype) <- generateFormPost orgForm
  defaultLayout $(widgetFile "createorganisation")

postCreateOrganisationR :: Handler ()
postCreateOrganisationR = do
  aid <- requireAuthId
  ((result, _), _) <- runFormPost orgForm
  case result of
   FormSuccess orgname -> do
     volumeuuid <- liftIO $ nextRandom
     let createvolumep = shell $ "docker create --name \""++(show volumeuuid)++"\" -v /home/jovyan jupyter/datascience-notebook /bin/true"
     _ <- liftIO $ readCreateProcess createvolumep ""
     oid <- runDB $ do
       orgid <- insert $ Organisation orgname aid (show volumeuuid)
       insert $ OrganisationUser orgid aid
       let insertrprofile = shell $ "docker run -t --volumes-from "++(show volumeuuid)++" -v /home/fergie/src/cirrus/jupyter-env/files:/tmp/files jupyter/datascience-notebook sh -c 'cp /tmp/files/Rprofile /home/jovyan/.Rprofile'"
       _ <- liftIO $ readCreateProcess insertrprofile ""
       return orgid
     -- also to do here:
     --   Insert .Rprofile and .pythonrc into data volume
     --      Store them in s3 and then just wget them into the volume
     --   create database (if not exist)
     --      unsure of how to do this.
     --      probably dockerise the whole thing?
     --      but how to do updates?
     --   create user and password for database
     --      this is a readonly user
     redirect $ OrganisationR oid
   _ -> do
     setMessage "Error in form"
     redirect CreateOrganisationR

orgForm :: Form Text
orgForm = renderDivs $ id <$> areq textField "Organisation Name" Nothing

getOrganisationR :: OrganisationId -> Handler Html
getOrganisationR orgid = do
  accounts <- runDB $ selectList [AccountOrganisation ==. orgid] []
  defaultLayout $(widgetFile "organisation")
