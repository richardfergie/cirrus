module Handler.User where

import Import
import System.Process
import Data.UUID.V4
import qualified Crypto.Nonce as Nonce
import qualified Data.Text as T
import Control.Concurrent (forkIO)

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
     let createvolumep = shell $ "docker create --name \""++(show volumeuuid)++"\" -v /home/jovyan notebook /bin/true"
     _ <- liftIO $ readCreateProcess createvolumep ""
     oid <- runDB $ do
       orgid <- insert $ Organisation orgname aid (show volumeuuid)
       _ <- insert $ OrganisationUser orgid aid
       let insertrprofile = "docker run -t --volumes-from "++(show volumeuuid)++" notebook sh -c 'cp /opt/defaults/Rprofile /home/jovyan/.Rprofile'"
       $(logWarn) (T.pack insertrprofile)
       _ <- liftIO $ readCreateProcess (shell insertrprofile) ""
       return orgid
     -- also to do here:
     --   Insert .Rprofile and .pythonrc into data volume
     --      Store them in s3 and then just wget them into the volume
     --      No - use docker volumes
     --   Database created at a later stage
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
  defaultLayout $ do
    setTitle "Add Account"
    $(widgetFile "createaccount")

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
     pgpass <- fmap (postgresDBPassword . secrets . appSettings) getYesod
     let dbcreate = "docker run -t --link cirrus-postgres:postgres -e PGPASSWORD=\""++pgpass++"\" dbscripts /scripts/newdb.sh \""++(T.unpack dbname)++"\" \""++(T.unpack dbuser)++"\" \""++(T.unpack dbpass)++"\""
     $(logWarn) $ T.pack dbcreate
     _ <- liftIO $ readCreateProcess (shell dbcreate) ""
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
     scrts <- fmap (secrets . appSettings) getYesod
     _ <- liftIO $ forkIO $ downloadAndInsertReports (T.unpack clientid) scrts
     setMessage "Your data is being imported. This may take a little while"
     redirect $ OrganisationR orgid
   _ -> do
     setMessage "There was an error in the form. Account not created"
     redirect $ CreateAccountR orgid

downloadAndInsertReports :: String -> Secrets -> IO ()
downloadAndInsertReports clientid secrets = do
  let downloadreports = shell $ "docker run -t --volumes-from report-data -e ADWORDS_CLIENT_ID=\""++(adwordsClientId secrets)++"\" -e ADWORDS_CLIENT_SECRET=\""++(adwordsClientSecret secrets)++"\" -e ADWORDS_DEVELOPER_TOKEN=\""++(adwordsDeveloperToken secrets)++"\" -e ADWORDS_REFRESH_TOKEN=\""++(adwordsRefreshToken secrets)++"\" -e CLIENT_ID=\""++clientid++"\" report-downloader python /download.py --all-time"
  _ <- readCreateProcess downloadreports ""
  let insertReports = shell $ "docker run --label \"type=tmp\" -t --volumes-from report-data --link cirrus-postgres:postgres -e PGPASSWORD=\""++(postgresDBPassword secrets)++"\" -e CLIENT_ID=\""++clientid++"\" reportinsert /report-insert.sh"
  _ <- readCreateProcess insertReports ""
  return ()
