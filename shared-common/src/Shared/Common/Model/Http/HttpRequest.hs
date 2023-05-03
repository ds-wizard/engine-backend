module Shared.Common.Model.Http.HttpRequest where

import Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as M

data HttpRequest = HttpRequest
  { requestMethod :: String
  , requestUrl :: String
  , requestHeaders :: M.Map String String
  , requestBody :: BS.ByteString
  , multipart :: Maybe HttpRequestMultipart
  }
  deriving (Show, Eq)

data HttpRequestMultipart = HttpRequestMultipart
  { key :: String
  , fileName :: Maybe String
  , contentType :: Maybe String
  }
  deriving (Show, Eq)
