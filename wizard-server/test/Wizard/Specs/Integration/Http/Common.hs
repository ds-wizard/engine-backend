module Wizard.Specs.Integration.Http.Common where

import Data.Aeson
import qualified Data.Set as S
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
    , responseOriginalRequest =
        Request
          { method = "POST"
          , host = "localhost"
          , port = 3000
          , secure = False
          , path = "/"
          , queryString = ""
          , requestHeaders = []
          , requestBody = ""
          , redirectCount = 0
          , proxy = Nothing
          , hostAddress = Nothing
          , rawBody = False
          , decompress = const True
          , checkResponse = \_ _ -> return ()
          , responseTimeout = ResponseTimeoutNone
          , cookieJar = Nothing
          , requestVersion = http20
          , onRequestBodyException = \_ -> return ()
          , requestManagerOverride = Nothing
          , shouldStripHeaderOnRedirect = const True
          , proxySecureMode = ProxySecureWithConnect
          , redactHeaders = S.empty
          }
    }
