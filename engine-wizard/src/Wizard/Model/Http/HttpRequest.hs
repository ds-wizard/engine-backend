module Wizard.Model.Http.HttpRequest where

import Data.ByteString.Char8 as BS
import Data.Map (Map)

data HttpRequest =
  HttpRequest
    { _httpRequestRequestMethod :: String
    , _httpRequestRequestUrl :: String
    , _httpRequestRequestHeaders :: Map String String
    , _httpRequestRequestBody :: BS.ByteString
    , _httpRequestMultipartFileName :: Maybe String
    }
  deriving (Show, Eq)
