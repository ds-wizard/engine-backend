module Registry.Util.Sentry where

import Control.Exception (SomeException)
import Data.Aeson (Value(..))
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as T
import qualified Network.Wai as WAI
import Network.Wai.Handler.Warp (defaultOnException)
import System.Log.Raven (initRaven, register, stderrFallback)
import System.Log.Raven.Transport.HttpConduit (sendRecord)
import System.Log.Raven.Types (SentryLevel(Error), SentryRecord(..), SentryService)

createSentryService :: String -> IO SentryService
createSentryService sentryUrl = initRaven sentryUrl id sendRecord stderrFallback

sentryOnException :: String -> SentryService -> Maybe WAI.Request -> SomeException -> IO ()
sentryOnException buildVersion sentryService mRequest exception = do
  register sentryService "webServerLogger" Error (show exception) (recordUpdate buildVersion mRequest exception)
  defaultOnException mRequest exception

recordUpdate :: String -> Maybe WAI.Request -> SomeException -> SentryRecord -> SentryRecord
recordUpdate buildVersion Nothing exception record = record
recordUpdate buildVersion (Just request) exception record =
  record
    { srCulprit = Just $ BS.unpack $ WAI.rawPathInfo request
    , srServerName = fmap BS.unpack $ WAI.requestHeaderHost request
    , srRelease = Just buildVersion
    , srExtra = HashMap.fromList [("request", String . T.pack . show $ request)]
    , srTags =
        HashMap.fromList
          [ ("method", BS.unpack $ WAI.requestMethod request)
          , ("url", BS.unpack $ WAI.rawPathInfo request)
          , ("query_string", BS.unpack $ WAI.rawQueryString request)
          ]
    }
