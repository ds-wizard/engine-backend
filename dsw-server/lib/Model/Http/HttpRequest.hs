module Model.Http.HttpRequest where

import Data.Map (Map)

data HttpRequest = HttpRequest
  { _httpRequestRequestMethod :: String
  , _httpRequestRequestUrl :: String
  , _httpRequestRequestHeaders :: Map String String
  , _httpRequestRequestBody :: String
  } deriving (Show, Eq)
