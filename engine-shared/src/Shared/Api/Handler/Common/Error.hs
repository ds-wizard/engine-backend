module Shared.Api.Handler.Common.Error where

import Control.Lens ((^.))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Reader (MonadReader, ask)
import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Network.HTTP.Types.Status
import Servant (ServerError(..), err302, err400, err401, err401, err403, err404, err500, errBody, errHeaders)

import Shared.Api.Resource.Error.ErrorDTO
import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Constant.Api (contentTypeHeaderJSON)
import Shared.Localization.Locale
import Shared.Model.Context.ContextLenses
import Shared.Model.Error.Error
import Shared.Util.Logger

sendError :: (MonadLogger m, MonadReader s m, HasLocalization' s, MonadIO m) => AppError -> m ServerError
sendError AcceptedError =
  return $
  ServerError
    { errHTTPCode = 202
    , errReasonPhrase = "Accepted"
    , errBody = encode AcceptedErrorDTO
    , errHeaders = [contentTypeHeaderJSON]
    }
sendError (FoundError url) =
  return $
  err302 {errBody = encode $ FoundErrorDTO url, errHeaders = [contentTypeHeaderJSON, ("Location", BS.pack url)]}
sendError (ValidationError formErrorRecords fieldErrorRecords) = do
  context <- ask
  let ls = context ^. localization'
  let formErrors = fmap (locale ls) formErrorRecords
  let localeTuple (k, v) = (k, locale ls v)
  let fieldErrors = fmap localeTuple fieldErrorRecords
  return $ err400 {errBody = encode $ ValidationErrorDTO formErrors fieldErrors, errHeaders = [contentTypeHeaderJSON]}
sendError (UserError localeRecord) = do
  context <- ask
  let ls = context ^. localization'
  let message = locale ls localeRecord
  return $ err400 {errBody = encode $ UserErrorDTO message, errHeaders = [contentTypeHeaderJSON]}
sendError (UnauthorizedError localeRecord) = do
  context <- ask
  let ls = context ^. localization'
  let message = locale ls localeRecord
  return $ err401 {errBody = encode $ UnauthorizedErrorDTO message, errHeaders = [contentTypeHeaderJSON]}
sendError (ForbiddenError localeRecord) = do
  context <- ask
  let ls = context ^. localization'
  let message = locale ls localeRecord
  return $ err403 {errBody = encode $ ForbiddenErrorDTO message, errHeaders = [contentTypeHeaderJSON]}
sendError (NotExistsError localeRecord) = do
  context <- ask
  let ls = context ^. localization'
  let message = locale ls localeRecord
  return $ err404 {errBody = encode $ NotExistsErrorDTO message, errHeaders = [contentTypeHeaderJSON]}
sendError (GeneralServerError errorMessage) = do
  logError _CMP_API errorMessage
  return $ err500 {errBody = encode $ GeneralServerErrorDTO errorMessage, errHeaders = [contentTypeHeaderJSON]}
sendError (HttpClientError status message) = do
  logError _CMP_API message
  return $
    ServerError
      { errHTTPCode = statusCode status
      , errReasonPhrase = BS.unpack . statusMessage $ status
      , errBody = BSL.pack message
      , errHeaders = [contentTypeHeaderJSON]
      }

sendErrorDTO :: MonadLogger m => ErrorDTO -> m ServerError
sendErrorDTO AcceptedErrorDTO =
  return $
  ServerError
    { errHTTPCode = 202
    , errReasonPhrase = "Accepted"
    , errBody = encode AcceptedErrorDTO
    , errHeaders = [contentTypeHeaderJSON]
    }
sendErrorDTO (FoundErrorDTO url) =
  return $
  err302 {errBody = encode $ FoundErrorDTO url, errHeaders = [contentTypeHeaderJSON, ("Location", BS.pack url)]}
sendErrorDTO (ValidationErrorDTO formErrors fieldErrors) =
  return $ err400 {errBody = encode $ ValidationErrorDTO formErrors fieldErrors, errHeaders = [contentTypeHeaderJSON]}
sendErrorDTO (UserErrorDTO message) =
  return $ err400 {errBody = encode $ UserErrorDTO message, errHeaders = [contentTypeHeaderJSON]}
sendErrorDTO (UnauthorizedErrorDTO message) =
  return $ err401 {errBody = encode $ UnauthorizedErrorDTO message, errHeaders = [contentTypeHeaderJSON]}
sendErrorDTO (ForbiddenErrorDTO message) =
  return $ err403 {errBody = encode $ ForbiddenErrorDTO message, errHeaders = [contentTypeHeaderJSON]}
sendErrorDTO (NotExistsErrorDTO message) =
  return $ err404 {errBody = encode $ NotExistsErrorDTO message, errHeaders = [contentTypeHeaderJSON]}
sendErrorDTO (GeneralServerErrorDTO message) = do
  logError _CMP_API message
  return $ err500 {errBody = encode $ GeneralServerErrorDTO message, errHeaders = [contentTypeHeaderJSON]}
sendErrorDTO (HttpClientErrorDTO status message) = do
  logError _CMP_API message
  return $
    ServerError
      { errHTTPCode = statusCode status
      , errReasonPhrase = BS.unpack . statusMessage $ status
      , errBody = BSL.pack message
      , errHeaders = [contentTypeHeaderJSON]
      }
