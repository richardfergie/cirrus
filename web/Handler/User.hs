module Handler.User where

import Import
import System.Process
import Prelude(init) --unsafe!!
import Data.UUID.V4
import qualified Crypto.Nonce as Nonce

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

createAccountForm :: [Entity Database] -> Form (Text,DatabaseId,Text)
createAccountForm [] = error "No databases exist"
createAccountForm dbs = renderDivs $ (\clientid dbid description -> (clientid,dbid,description)) <$>
  areq textField "AdWords Client ID" (Just "XXX-XXX-XXXX") <*>
  areq (selectFieldList databases) "Database" Nothing <*>
  areq textField "Description" Nothing
  where databases = fmap (\x -> (databaseDescription $ entityVal x,entityKey x)) dbs

createDatabaseForm :: Form Text
createDatabaseForm = renderDivs $ id <$> areq textField "Description" Nothing

getCreateAccountR :: OrganisationId -> Handler Html
getCreateAccountR orgid = do
  databases <- runDB $ selectList [DatabaseOrganisation ==. orgid] []
  (accountform, accountenctype) <- generateFormPost $ createAccountForm databases
  (databaseform,databaseenctype) <- generateFormPost createDatabaseForm
  defaultLayout $(widgetFile "createaccount")

postCreateDatabaseR :: OrganisationId -> Handler ()
postCreateDatabaseR orgid = do
  ((result, _), _) <- runFormPost createDatabaseForm
  case result of
   FormSuccess dbdescription -> do
     gen <- liftIO $ Nonce.new
     dbname <- liftIO $ Nonce.nonce128urlT gen
     dbuser <- liftIO $ Nonce.nonce128urlT gen
     dbpass <- liftIO $ Nonce.nonce128urlT gen
     now <- liftIO $ getCurrentTime
     -- do some docker magic to create the database
     _ <- runDB $ insert $ Database orgid dbname dbuser dbpass dbdescription now
     redirect $ CreateAccountR orgid
   _ -> do
     setMessage "Error in form. Failed to create database"
     redirect $ CreateAccountR orgid

postCreateAccountR :: OrganisationId -> Handler ()
postCreateAccountR orgid = do
  databases <- runDB $ selectList [DatabaseOrganisation ==. orgid] []
  ((result, _), _) <- runFormPost (createAccountForm databases)
  case result of
   FormSuccess (clientid,dbid,description) -> do
     now <- liftIO $ getCurrentTime
     _ <- runDB $ insert $ Account orgid clientid dbid description now
     setMessage "Your data is being imported. This may take a little while"
     redirect $ OrganisationR orgid
   _ -> do
     setMessage "There was an error in the form. Account not created"
     redirect $ CreateAccountR orgid
