module Wizard.Util.Sentry where

import Control.Exception (SomeException)
import Data.Aeson (Value(..))
import qualified Data.ByteString.Char8 as BS
import qualified Data.CaseInsensitive as CI
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as L
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Network.Wai as WAI
import Network.Wai.Handler.Warp (defaultOnException)
import System.Log.Raven (initRaven, register, stderrFallback)
import System.Log.Raven.Transport.HttpConduit (sendRecord)
import System.Log.Raven.Types (SentryLevel(Error), SentryRecord(..), SentryService)

import Shared.Util.Token
import Wizard.Service.Token.TokenService

createSentryService :: String -> IO SentryService
createSentryService sentryUrl = initRaven sentryUrl id sendRecord stderrFallback

sentryOnException :: String -> SentryService -> Maybe WAI.Request -> SomeException -> IO ()
sentryOnException buildVersion sentryService mRequest exception = do
  register sentryService "webServerLogger" Error (show exception) (recordUpdate buildVersion mRequest exception)
  defaultOnException mRequest exception

recordUpdate :: String -> Maybe WAI.Request -> SomeException -> SentryRecord -> SentryRecord
recordUpdate buildVersion Nothing exception record = record
recordUpdate buildVersion (Just request) exception record =
  let headersWithoutAuthorization = filter (not . isAuthorizationHeader) . WAI.requestHeaders $ request
      isAuthorizationHeader (headerName, value) = CI.mk "Authorization" == headerName
      authorizationHeader =
        fmap (\(_, value) -> BS.unpack value) . L.find isAuthorizationHeader . WAI.requestHeaders $ request
      sanitizedRequest = request {WAI.requestHeaders = headersWithoutAuthorization}
   in record
        { srCulprit = Just $ BS.unpack $ WAI.rawPathInfo request
        , srServerName = fmap BS.unpack $ WAI.requestHeaderHost request
        , srRelease = Just buildVersion
        , srExtra =
            HashMap.fromList
              [ ( "user"
                , String . T.pack . fromMaybe "" . getUserUuidFromToken . fromMaybe "" . separateToken . fromMaybe "" $
                  authorizationHeader)
              , ("request", String . T.pack . show $ sanitizedRequest)
              ]
        , srTags =
            HashMap.fromList
              [ ("method", BS.unpack $ WAI.requestMethod request)
              , ("url", BS.unpack $ WAI.rawPathInfo request)
              , ("query_string", BS.unpack $ WAI.rawQueryString request)
              ]
        }
