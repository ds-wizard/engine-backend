module Shared.Common.Util.Sentry where

import Control.Exception (SomeException (..), fromException)
import Data.Aeson (Value (..), toJSON)
import qualified Data.ByteString.Char8 as BS
import qualified Data.CaseInsensitive as CI
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as L
import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Text as T
import Data.Typeable (typeOf)
import qualified Network.Wai as WAI
import Network.Wai.Handler.Warp (defaultOnException)
import System.Log.Raven (initRaven, register, stderrFallback)
import System.Log.Raven.Transport.HttpConduit (sendRecord)
import System.Log.Raven.Types (SentryLevel (Error), SentryRecord (..), SentryService)
import System.TimeManager (TimeoutThread (..))

createSentryService :: String -> IO SentryService
createSentryService sentryUrl = initRaven sentryUrl id sendRecord stderrFallback

-- Ignore timeout exception (https://magnus.therning.org/2021-07-03-the-timeout-manager-exception.html)
sentryOnException :: String -> (Maybe String -> [(String, Value)]) -> SentryService -> Maybe WAI.Request -> SomeException -> IO ()
sentryOnException buildVersion getSentryIdentity sentryService mRequest exception
  | Just TimeoutThread <- fromException exception = return ()
  | otherwise = do
      register sentryService "webServerLogger" Error (format exception) (recordUpdate buildVersion mRequest exception getSentryIdentity)
      defaultOnException mRequest exception

format :: SomeException -> String
format (SomeException exception) = show exception

getExceptionType :: SomeException -> String
getExceptionType (SomeException exception) = show . typeOf $ exception

recordUpdate :: String -> Maybe WAI.Request -> SomeException -> (Maybe String -> [(String, Value)]) -> SentryRecord -> SentryRecord
recordUpdate buildVersion Nothing exception getSentryIdentity record = record
recordUpdate buildVersion (Just request) exception getSentryIdentity record =
  let headersWithoutAuthorization = filter (not . isAuthorizationHeader) . WAI.requestHeaders $ request
      isAuthorizationHeader (headerName, value) = CI.mk "Authorization" == headerName
      isTraceUuidHeader (headerName, value) = CI.mk "x-trace-uuid" == headerName
      isHostHeader (headerName, value) = CI.mk "Host" == headerName
      authorizationHeader =
        fmap (\(_, value) -> BS.unpack value) . L.find isAuthorizationHeader . WAI.requestHeaders $ request
      traceUuidHeader = fmap (\(_, value) -> BS.unpack value) . L.find isTraceUuidHeader . WAI.requestHeaders $ request
      hostHeader = fmap (\(_, value) -> BS.unpack value) . L.find isHostHeader . WAI.requestHeaders $ request
      sanitizedRequest = request {WAI.requestHeaders = headersWithoutAuthorization}
      url = T.pack . BS.unpack . WAI.rawPathInfo $ request
      method = T.pack . BS.unpack . WAI.requestMethod $ request
      queryString = T.pack . BS.unpack . WAI.rawQueryString $ request
      host = T.pack . fromMaybe "" $ hostHeader
      exceptionType = T.pack . getExceptionType $ exception
      exceptionValue = T.pack . format $ exception
   in record
        { srPlatform = Just "haskell"
        , srCulprit = Just $ BS.unpack $ WAI.rawPathInfo request
        , srServerName = fmap BS.unpack $ WAI.requestHeaderHost request
        , srRelease = Just buildVersion
        , srInterfaces =
            HashMap.fromList
              ( [
                  ( "sentry.interfaces.Http"
                  , toJSON $
                      HashMap.fromList
                        [ ("url", String url)
                        , ("method", String method)
                        ,
                          ( "query_string"
                          , if queryString == ""
                              then Null
                              else String queryString
                          )
                        , ("headers", toJSON [("Host", host)])
                        ]
                  )
                ,
                  ( "sentry.interfaces.Exception"
                  , toJSON $ HashMap.fromList [("type", String exceptionType), ("value", String exceptionValue)]
                  )
                ]
                  ++ getSentryIdentity authorizationHeader
              )
        , srTags =
            if isNothing traceUuidHeader
              then HashMap.empty
              else HashMap.fromList [("traceUuid", fromMaybe "" traceUuidHeader)]
        , srExtra = HashMap.fromList [("request", String . T.pack . show $ sanitizedRequest)]
        }
