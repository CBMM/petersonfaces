{-# language DataKinds #-}
{-# language OverloadedStrings #-}
{-# language TypeOperators #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language DeriveGeneric #-}

module Dropbox where

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import Data.Char (toLower)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Default
import Data.Proxy
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Data.Time
import GHC.Generics
import GHC.TypeLits
import Reflex.Dom
import Servant.API
import Servant.Reflex

listFolder :: forall t m.MonadWidget t m => Event t T.Text -> m (Event t (ReqResult a))
listFolder = client 


type DropboxApi = "2" :> "files" :> Header "Authorization" DropboxBearer :>
  ("list_folder" :> ReqBody '[JSON]  :> Post '[JSON] DBEntries)

type DropboxContentAPI a = "2" :> "files" :> Header "Authorization" DropboxBearer :>
  ("download"   :> Header "Dropbox-API-Arg" DBPath       :> Post '[JSON] a
  :<|> "upload" :> Header "Dropbox-API-Arg" DBUploadArgs :> ReqBody '[OctetStream] a :> Post '[JSON] DBEntry)

dbApi :: Proxy DropboxApi
dbApi = Proxy

data DropboxBearer = DBBearer T.Text

instance ToHttpApiData DropboxBearer where
  toQueryParam (DBBearer t) = "Bearer " <> t

data DBEntry = DBEntry
  { _dbeName            :: T.Text
  , _dbePath_lower      :: T.Text
  , _dbeId              :: T.Text
  , _dbeClient_modified :: UTCTime
  , _dbeServer_modified :: UTCTime
  , _dbeRev             :: T.Text
  , _dbeSize            :: Int
  } deriving (Show, Generic)

instance A.FromJSON DBEntry where
  parseJSON = A.genericParseJSON A.defaultOptions { A.fieldLabelModifier = fmap toLower . drop 4 }


data DBPath = DBPath {
  _dbpPath :: T.Text
  } deriving (Generic)

instance A.ToJSON DBPath where
  toJSON = A.genericToJSON A.defaultOptions { A.fieldLabelModifier = fmap toLower . drop 4 }

instance ToHttpApiData DBPath where
  -- toQueryParam (DBBearer t) = "Bearer " <> t
  toQueryParam = E.decodeUtf8 . BSL.toStrict . A.encode

data DBEntries = DBEntries {
    _dbesEntries :: [DBEntry]
  , _dbesCursor :: T.Text
  , _dbesHas_more :: Bool
  } deriving (Generic, Show)

instance A.FromJSON DBEntries where
  parseJSON = A.genericParseJSON A.defaultOptions { A.fieldLabelModifier = fmap toLower . drop 5 }

data DBUploadArgs = DBUploadArgs {
    _dbupPath       :: T.Text
  , _dbupMode       :: UploadMode
  , _dbupAutorename :: Bool
  , _dbupMute       :: Bool
  } deriving (Eq, Ord, Show, Generic)

data UploadMode =  Upload_add | Upload_overwrite | Upload_update
  deriving (Eq, Show, Read, Ord, Generic)

instance A.ToJSON UploadMode where
  toJSON = A.String . T.drop 7 . T.pack . show

instance ToHttpApiData DBUploadArgs where
  toQueryParam = E.decodeUtf8 . BSL.toStrict . A.encode

instance A.ToJSON DBUploadArgs where
  toJSON = A.genericToJSON A.defaultOptions { A.fieldLabelModifier = fmap toLower . drop 5 }

instance Default DBUploadArgs where
  def = DBUploadArgs "/file.txt" Upload_add True False

instance A.ToJSON a => MimeRender OctetStream a where
  mimeRender _ val = A.encode val

