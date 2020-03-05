module Shared.Api.Handler.Common where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.List.NonEmpty as NE
import Data.Typeable
import Servant (Accept(..), MimeRender(..), MimeUnrender(..))

data ApplicationJavascript
  deriving (Typeable)

instance Accept ApplicationJavascript where
  contentTypes _ =
    "application/javascript" NE.:| ["application/javascript; charset=utf-8", "application/javascript;charset=utf-8"]

instance MimeRender ApplicationJavascript String where
  mimeRender _ = BS.pack

instance MimeUnrender ApplicationJavascript String where
  mimeUnrender _ = Right . BS.unpack

-- ------------------------------------------------------------------------
data JSONPlain
  deriving (Typeable)

instance Accept JSONPlain where
  contentTypes _ = "application/json" NE.:| ["application/json; charset=utf-8", "application/json;charset=utf-8"]

instance MimeRender JSONPlain String where
  mimeRender _ = BS.pack

instance MimeUnrender JSONPlain String where
  mimeUnrender _ = Right . BS.unpack

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
