{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Database.Persist
import Database.Persist.Sql
import Database.Persist.Quasi
import Database.Persist.TH
import Database.Persist.Postgresql
import Data.Csv
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Control.Monad.Reader
import Control.Applicative
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as C
import qualified Data.Vector as V
import Control.Monad.Logger
import Control.Monad.Trans.Resource
import System.Environment(getArgs,lookupEnv)
import Data.Time.Format
import qualified Codec.Compression.GZip as GZip
import Shared.Types

share [mkPersist sqlSettings, mkMigrate "migrateAdWords"]
    $(persistFileWith lowerCaseSettings "../database/persistent-model")

type WithAccount a = Text -> a
type WithAccountTime a = Text -> UTCTime -> a

convertWithAccount :: Text -> V.Vector (WithAccount a) -> V.Vector a
convertWithAccount accid vs = V.map ($ accid) vs

convertWithAccountTime :: Text -> UTCTime -> V.Vector (WithAccountTime a) -> V.Vector a
convertWithAccountTime accid now vs = V.map (($ (accid,now)) . uncurry) vs

instance FromField Day where
  parseField s = case (C.split '-' s) of
    [yyyy,mm,dd] -> fmap (\[x,y,z] -> fromGregorian (toInteger x) y z) $
                      mapM parseField [yyyy,mm,dd]
    otherwise -> mzero

instance FromNamedRecord (WithAccount CampaignStructure) where
  parseNamedRecord m = (\x y z-> CampaignStructure z x y)
     <$> m .: "Campaign ID"
     <*> m .: "Campaign"

instance FromNamedRecord (WithAccount AdGroupStructure) where
  parseNamedRecord m = (\a b c d -> AdGroupStructure d a b c)
    <$> m .: "Campaign ID"
    <*> m .: "Ad group ID"
    <*> m .: "Ad group"

instance FromNamedRecord (WithAccount TextAdStructure) where
  parseNamedRecord m = (\a b c d e f g h i -> TextAdStructure i a b c d e f g h)
    <$> m .: "Campaign ID"
    <*> m .: "Ad group ID"
    <*> m .: "Ad ID"
    <*> m .: "Ad"
    <*> m .: "Description line 1"
    <*> m .: "Description line 2"
    <*> m .: "Display URL"
    <*> m .: "Destination URL"

instance FromNamedRecord (WithAccount KeywordStructure) where
  parseNamedRecord m = (\a b c d e f -> KeywordStructure f a b c d e)
    <$> m .: "Campaign ID"
    <*> m .: "Ad group ID"
    <*> m .: "Keyword ID"
    <*> m .: "Keyword"
    <*> m .: "Match type"

instance FromNamedRecord (WithAccountTime CampaignAttributes) where
  parseNamedRecord m = (\a b c d -> CampaignAttributes c a b d)
    <$> m .: "Campaign ID"
    <*> m .: "Campaign state"

instance FromNamedRecord (WithAccountTime AdGroupAttributes) where
  parseNamedRecord m = (\a b c d e f -> AdGroupAttributes e a b c d f)
    <$> m .: "Campaign ID"
    <*> m .: "Ad group ID"
    <*> m .: "Ad group state"
    <*> m .: "Default max. CPC"

instance FromNamedRecord (WithAccountTime TextAdAttributes) where
  parseNamedRecord m = (\a b c d e -> TextAdAttributes e a b c d)
    <$> m .: "Campaign ID"
    <*> m .: "Ad group ID"
    <*> m .: "Ad ID"
    <*> m .: "Ad state"

instance FromNamedRecord (WithAccountTime KeywordAttributes) where
  parseNamedRecord m = (\a b c d e f g h i -> KeywordAttributes h a b c d e f g i)
    <$> m .: "Campaign ID"
    <*> m .: "Ad group ID"
    <*> m .: "Keyword ID"
    <*> m .: "Max. CPC"
    <*> m .: "Quality score"
    <*> m .: "First page CPC"
    <*> m .: "Top of page CPC"

instance FromNamedRecord (WithAccountTime CampaignPerformance) where
  parseNamedRecord m = (\a b c d e f g h i j -> CampaignPerformance i a b c d e f g h j)
    <$> m .: "Campaign ID"
    <*> m .: "Day"
    <*> m .: "Network (with search partners)"
    <*> m .: "Clicks"
    <*> m .: "Impressions"
    <*> m .: "Cost"
    <*> m .: "Avg. position"
    <*> m .: "Converted clicks"

instance FromNamedRecord (WithAccountTime AdGroupPerformance) where
  parseNamedRecord m = (\a b c d e f g h i j k -> AdGroupPerformance j a b c d e f g h i k)
    <$> m .: "Campaign ID"
    <*> m .: "Ad group ID"
    <*> m .: "Day"
    <*> m .: "Network (with search partners)"
    <*> m .: "Clicks"
    <*> m .: "Impressions"
    <*> m .: "Cost"
    <*> m .: "Avg. position"
    <*> m .: "Converted clicks"

instance FromNamedRecord (WithAccountTime TextAdPerformance) where
  parseNamedRecord m = (\a b c d e f g h i j k l m -> TextAdPerformance l a b c d e f g h i j k m)
    <$> m .: "Campaign ID"
    <*> m .: "Ad group ID"
    <*> m .: "Ad ID"
    <*> m .: "Keyword ID"
    <*> m .: "Day"
    <*> m .: "Network (with search partners)"
    <*> m .: "Clicks"
    <*> m .: "Impressions"
    <*> m .: "Cost"
    <*> m .: "Avg. position"
    <*> m .: "Converted clicks"

reportDir :: String
reportDir = "/tmp/"

accountid = "929-872-4012"
testConf = Conf "/tmp/" "929-872-4012"

readArgs = do
  args <- getArgs
  case args of
   [dir,account] -> return (dir, account)
   otherwise -> error "Need two arguments"

makeConnectionString = do
  dbhost <- fmap (maybe "127.0.0.1" id) $ lookupEnv "POSTGRES_PORT_5432_TCP_ADDR"
  dbport <- fmap (maybe "5432" id) $ lookupEnv "POSTGRES_PORT_5432_TCP_PORT"
  dbpassd <- fmap (maybe "adwords" id) $ lookupEnv "ADWPASSWD"
  let str = ["host=",dbhost," port=",dbport," user=adwords dbname=adwords password=",dbpassd]
  return $ C.pack $ concat str

main :: IO ()
main = do
  (dir, account) <- readArgs
  connstr <- makeConnectionString
  now <- getCurrentTime
  runNoLoggingT $ withPostgresqlConn connstr $ \conn ->
     liftIO $ flip runSqlPersistM conn $ do
       runMigration migrateAdWords
       transactionSave
       uploadStructureReport $ Conf dir account "CampaignStructure" insertCampaignStructure
       uploadStructureReport $ Conf dir account "AdGroupStructure" insertAdGroupStructure
       uploadStructureReport $ Conf dir account "TextAdStructure" insertTextAdStructure
       uploadStructureReport $ Conf dir account "KeywordStructure" insertKeywordStructure
       uploadOtherReport $ Conf dir account "CampaignAttribute"
         (\x -> insert (x::CampaignAttributes) >> return ())
       uploadOtherReport $ Conf dir account "AdGroupAttribute"
         (\x -> insert (x::AdGroupAttributes) >> return ())
       uploadOtherReport $ Conf dir account "TextAdAttribute"
         (\x -> insert (x::TextAdAttributes) >> return ())
       uploadOtherReport $ Conf dir account "KeywordAttribute"
         (\x -> insert (x::KeywordAttributes) >> return ())
       uploadOtherReport $ Conf dir account "CampaignPerformance"
         (\x -> insert (x::CampaignPerformance) >> return ())
       uploadOtherReport $ Conf dir account "AdGroupPerformance"
         (\x -> insert (x::AdGroupPerformance) >> return ())
       uploadOtherReport $ Conf dir account "TextAdPerformance"
         (\x -> insert (x::TextAdPerformance) >> return ())

data ReportUploadConfig func = Conf {
  csvDirectory :: String,
  accountId :: String,
  reportName :: String,
  uploadFunction :: func
                               }
generateFilename dir aid report t = concat [dir,aid,":",report,":",datestring]
  where datestring = formatTime defaultTimeLocale "%Y-%m-%d" t

insertStructure
  :: (PersistUnique (PersistEntityBackend val), PersistEntity val) =>
     (val -> Unique val)
     -> val -> ReaderT (PersistEntityBackend val) (NoLoggingT (ResourceT IO)) ()
insertStructure uniqueness ent = do
  maybecs <- getBy $ uniqueness ent
  case maybecs of
   Nothing -> insert ent >> return ()
   Just e -> replace (entityKey e) ent

insertCampaignStructure = insertStructure
  (\e -> UniqueCampaignStructure (campaignStructureCampaignId e))

insertAdGroupStructure = insertStructure
  (\e -> UniqueAdGroupStructure (adGroupStructureAdGroupId e))

insertTextAdStructure = insertStructure
  (\e -> UniqueTextAdStructure (textAdStructureAdGroupId e) (textAdStructureAdId e))

insertKeywordStructure = insertStructure
  (\e -> UniqueKeywordStructure (keywordStructureAdGroupId e) (keywordStructureKeywordId e))

uploadStructureReport :: (PersistEntity val, FromNamedRecord (WithAccount val)) =>
  ReportUploadConfig
       (val
        -> ReaderT SqlBackend (NoLoggingT (ResourceT IO)) ())
        -> ReaderT SqlBackend (NoLoggingT (ResourceT IO)) ()
uploadStructureReport conf = do
  now <- liftIO $ getCurrentTime
  raw <- liftIO $ fmap GZip.decompress $ BL.readFile $ generateFilename
                                  (csvDirectory conf)
                                  (accountId conf)
                                  (reportName conf)
                                  now
  let eres = decodeByName raw
  case eres of
    Left err -> error err
    Right (_,res) -> do
      let ents = convertWithAccount accountid res
      V.mapM_ (uploadFunction conf) ents

uploadOtherReport :: (PersistEntity val, FromNamedRecord (WithAccountTime val)) =>
  ReportUploadConfig
       (val
        -> ReaderT SqlBackend (NoLoggingT (ResourceT IO)) ())
        -> ReaderT SqlBackend (NoLoggingT (ResourceT IO)) ()
uploadOtherReport conf = do
  now <- liftIO $ getCurrentTime
  raw <- liftIO $ fmap GZip.decompress $ BL.readFile $ generateFilename
                                  (csvDirectory conf)
                                  (accountId conf)
                                  (reportName conf)
                                  now
  let eres = decodeByName raw
  case eres of
    Left err -> error err
    Right (_,res) -> do
      let ents = convertWithAccountTime accountid now res
      V.mapM_ (uploadFunction conf) ents
