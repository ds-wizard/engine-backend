module Shared.Api.Handler.Common.Error where

import Control.Lens ((^.))
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Reader (MonadReader, ask, liftIO)
import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.HashMap.Strict as HashMap
import Network.HTTP.Types.Status
import Prelude hiding (log)
import Servant (ServerError(..), err302, err400, err401, err401, err403, err404, err500, errBody, errHeaders)
import System.Log.Raven (initRaven, register, stderrFallback)
import System.Log.Raven.Transport.HttpConduit (sendRecord)
import System.Log.Raven.Types (SentryLevel(Error), SentryRecord(..))

import LensesConfig
import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Constant.Api (contentTypeHeaderJSON)
import Shared.Model.Context.ContextLenses
import Shared.Model.Error.Error
import Shared.Util.Logger

sendError ::
     (MonadReader s m, HasServerConfig' s sc, HasBuildInfoConfig' s, MonadLogger m, MonadIO m)
  => AppError
  -> m ServerError
sendError AcceptedError =
  return $
  ServerError
    { errHTTPCode = 202
    , errReasonPhrase = "Accepted"
    , errBody = encode AcceptedError
    , errHeaders = [contentTypeHeaderJSON]
    }
sendError (FoundError url) =
  return $ err302 {errBody = encode $ FoundError url, errHeaders = [contentTypeHeaderJSON, ("Location", BS.pack url)]}
sendError (ValidationError formErrors fieldErrors) =
  return $ err400 {errBody = encode $ ValidationError formErrors fieldErrors, errHeaders = [contentTypeHeaderJSON]}
sendError (UserError message) =
  return $ err400 {errBody = encode $ UserError message, errHeaders = [contentTypeHeaderJSON]}
sendError (SystemLogError message) =
  return $ err400 {errBody = encode $ SystemLogError message, errHeaders = [contentTypeHeaderJSON]}
sendError (UnauthorizedError message) =
  return $ err401 {errBody = encode $ UnauthorizedError message, errHeaders = [contentTypeHeaderJSON]}
sendError (ForbiddenError message) =
  return $ err403 {errBody = encode $ ForbiddenError message, errHeaders = [contentTypeHeaderJSON]}
sendError (NotExistsError message) =
  return $ err404 {errBody = encode $ NotExistsError message, errHeaders = [contentTypeHeaderJSON]}
sendError LockedError =
  return $
  ServerError
    {errHTTPCode = 423, errReasonPhrase = "Locked", errBody = encode LockedError, errHeaders = [contentTypeHeaderJSON]}
sendError (GeneralServerError message) = do
  logError _CMP_API message
  sendToSentry message
  return $ err500 {errBody = encode $ GeneralServerError message, errHeaders = [contentTypeHeaderJSON]}
sendError (HttpClientError status message) = do
  logError _CMP_API message
  return $
    ServerError
      { errHTTPCode = statusCode status
      , errReasonPhrase = BS.unpack . statusMessage $ status
      , errBody = BSL.pack message
      , errHeaders = [contentTypeHeaderJSON]
      }

sendToSentry ::
     (MonadReader s m, HasServerConfig' s sc, HasBuildInfoConfig' s, MonadLogger m, MonadIO m) => String -> m ()
sendToSentry message = do
  context <- ask
  when
    (context ^. serverConfig' . sentry' . enabled)
    (do let sentryDsn = context ^. serverConfig' . sentry' . dsn
        sentryService <- liftIO $ initRaven sentryDsn id sendRecord stderrFallback
        let buildVersion = context ^. buildInfoConfig' . version
        let sentryError = f' "GeneralServerError: %s" [message]
        liftIO $ register sentryService "appLogger" Error sentryError (recordUpdate buildVersion))

recordUpdate :: String -> SentryRecord -> SentryRecord
recordUpdate buildVersion record =
  record {srRelease = Just buildVersion, srExtra = HashMap.empty, srTags = HashMap.empty}
