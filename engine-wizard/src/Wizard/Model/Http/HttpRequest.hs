module Wizard.Model.Http.HttpRequest where

import Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as M

data HttpRequest = HttpRequest
  { requestMethod :: String
  , requestUrl :: String
  , requestHeaders :: M.Map String String
  , requestBody :: BS.ByteString
  , multipartFileName :: Maybe String
  }
  deriving (Show, Eq)
