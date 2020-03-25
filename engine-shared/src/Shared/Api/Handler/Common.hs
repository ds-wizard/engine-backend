module Shared.Api.Handler.Common where

import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.List.NonEmpty as NE
import Data.Swagger (NamedSchema(..), ToSchema(..), binarySchema)
import Data.Typeable
import GHC.Generics
import Servant (Accept(..), MimeRender(..), MimeUnrender(..), OctetStream(..))

data ApplicationJavascript
  deriving (Typeable)

instance Accept ApplicationJavascript where
  contentTypes _ =
    "application/javascript" NE.:| ["application/javascript; charset=utf-8", "application/javascript;charset=utf-8"]

instance MimeRender ApplicationJavascript Javascript where
  mimeRender _ (Javascript content) = content

instance MimeUnrender ApplicationJavascript Javascript where
  mimeUnrender _ = Right . Javascript

newtype Javascript =
  Javascript BSL.ByteString
  deriving (Generic)

instance ToSchema Javascript where
  declareNamedSchema _ = return $ NamedSchema (Just "Javascript") binarySchema

-- ------------------------------------------------------------------------
data JSONPlain
  deriving (Typeable)

instance Accept JSONPlain where
  contentTypes _ = "application/json" NE.:| ["application/json; charset=utf-8", "application/json;charset=utf-8"]

instance MimeRender JSONPlain String where
  mimeRender _ = BSL.pack

instance MimeUnrender JSONPlain String where
  mimeUnrender _ = Right . BSL.unpack

-- ------------------------------------------------------------------------
data SafeJSON
  deriving (Typeable)

instance Accept SafeJSON where
  contentTypes _ = "application/json" NE.:| ["application/json; charset=utf-8", "application/json;charset=utf-8"]

instance {-# OVERLAPPABLE #-} ToJSON a => MimeRender SafeJSON a where
  mimeRender _ = encode

instance FromJSON a => MimeUnrender SafeJSON a where
  mimeUnrender _ content =
    case eitherDecode content of
      Right decoded -> Right decoded
      -- It should be retrieved from Localization Dictionary
      Left error -> Left "Problem in deserialization of JSON"

-- ------------------------------------------------------------------------
newtype FileStream =
  FileStream BS.ByteString
  deriving (Generic)

instance ToSchema FileStream where
  declareNamedSchema _ = return $ NamedSchema (Just "FileStream") binarySchema

instance MimeRender OctetStream FileStream where
  mimeRender _ (FileStream content) = BSL.fromStrict content
