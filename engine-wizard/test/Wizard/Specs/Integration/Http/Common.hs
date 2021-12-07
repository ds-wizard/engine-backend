module Wizard.Specs.Integration.Http.Common where

import Data.Aeson
import Network.HTTP.Client.Internal
import Network.HTTP.Types.Status
import Network.HTTP.Types.Version

createResponse body =
  Response
    { responseStatus = ok200
    , responseVersion = http11
    , responseHeaders = []
    , responseBody = encode body
    , responseCookieJar = undefined
    , responseClose' = undefined
    }
