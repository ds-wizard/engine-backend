module Wizard.Model.Http.HttpRequest where

import Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as M

data HttpRequest =
  HttpRequest
    { _httpRequestRequestMethod :: String
    , _httpRequestRequestUrl :: String
    , _httpRequestRequestHeaders :: M.Map String String
    , _httpRequestRequestBody :: BS.ByteString
    , _httpRequestMultipartFileName :: Maybe String
    }
  deriving (Show, Eq)
