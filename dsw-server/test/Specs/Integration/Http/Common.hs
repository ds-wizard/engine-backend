module Specs.Integration.Http.Common where

import Data.Aeson
import Data.HashMap.Strict as HM
import qualified Data.Vector as V
import Network.HTTP.Client.Internal
import Network.HTTP.Types.Status
import Network.HTTP.Types.Version

obj = Object . HM.fromList

arr = Array . V.fromList

createResponse body =
  Response
  { responseStatus = ok200
  , responseVersion = http11
  , responseHeaders = []
  , responseBody = encode body
  , responseCookieJar = undefined
  , responseClose' = undefined
  }
