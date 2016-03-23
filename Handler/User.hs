module Handler.User where

import Import
import System.Process
import Prelude(init) --unsafe!!

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
     let createvolumep = shell $ "docker create -v /home/jovyan jupyter/datascience-notebook /bin/true"
     volumeid <- fmap init $ liftIO $ readCreateProcess createvolumep ""
     oid <- runDB $ do
       orgid <- insert $ Organisation orgname aid volumeid
       insert $ OrganisationUser orgid aid
       return orgid
     -- also to do here:
     --   create data volume
     --      docker create -v /home/jovyan jupyter/datascience-notebook /bin/true
     --      and store resulting id for use in --volumes-from
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
getOrganisationR orgid = defaultLayout $(widgetFile "organisation")
