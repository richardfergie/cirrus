{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import Database.Persist
import Database.Persist.Sql
import Database.Persist.Quasi
import Database.Persist.TH
import Database.Persist.Postgresql
import qualified Data.ByteString.Char8 as C
import System.Environment(getArgs,lookupEnv)
import Control.Monad.Logger
import Control.Monad.Trans
import Data.Text(Text)
import Data.Time
import Shared.Types

share [mkPersist sqlSettings, mkMigrate "migrateAdWords"]
    $(persistFileWith lowerCaseSettings "../database/persistent-model")

makeConnectionString = do
  dbhost <- fmap (maybe "127.0.0.1" id) $ lookupEnv "POSTGRES_PORT_5432_TCP_ADDR"
  dbport <- fmap (maybe "5432" id) $ lookupEnv "POSTGRES_PORT_5432_TCP_PORT"
  dbpassd <- fmap (maybe "adwords" id) $ lookupEnv "ADWPASSWD"
  let str = ["host=",dbhost," port=",dbport," user=adwords dbname=adwords password=",dbpassd]
  return $ C.pack $ concat str

main :: IO ()
main = do
  connstr <- makeConnectionString
  runNoLoggingT $ withPostgresqlConn connstr $ \conn ->
     liftIO $ flip runSqlPersistM conn $ do
       runMigration migrateAdWords
