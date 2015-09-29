{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Shared.Types where

import Database.Persist
import Database.Persist.Sql
import Data.Csv
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as C
import Data.Char

newtype AdWordsMicros = Micros { unMicros :: Int} deriving (Eq, Num)

instance Show AdWordsMicros where
  show x = (++) "Currency " $ show $ flip (/) 100 $ fromIntegral $ round $ (fromIntegral $ unMicros x)/10000

instance PersistField AdWordsMicros where
   toPersistValue = PersistInt64 . fromIntegral . fromEnum . unMicros
   fromPersistValue (PersistInt64 c) = Right . Micros . toEnum . fromIntegral $ c
   fromPersistValue x = Left $ T.pack $ "Expected Int64 counting cents, got: " ++ show x

instance PersistFieldSql AdWordsMicros where
    sqlType _ = SqlInt64

instance FromField AdWordsMicros where
   parseField s = if ("auto" `C.isPrefixOf` s) -- auto bidding settings DISLIKE
     then fmap Micros $ parseField $ C.filter (isDigit) s
     else fmap Micros $ parseField s

instance FromField (Maybe AdWordsMicros) where
  parseField s = case (C.take 4 s) of
    " --" -> pure Nothing
    "auto" -> fmap (fmap Micros) $ parseField $ C.filter (isDigit) s
    otherwise -> fmap (Just . Micros) $ parseField s
