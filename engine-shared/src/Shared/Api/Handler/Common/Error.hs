module Shared.Api.Handler.Common.Error where

import Control.Monad.Logger (MonadLogger)
import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Network.HTTP.Types.Status
import Servant (ServerError(..), err302, err400, err401, err401, err403, err404, err500, errBody, errHeaders)

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Constant.Api (contentTypeHeaderJSON)
import Shared.Model.Error.Error
import Shared.Util.Logger

sendError :: MonadLogger m => AppError -> m ServerError
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
sendError (UnauthorizedError message) =
  return $ err401 {errBody = encode $ UnauthorizedError message, errHeaders = [contentTypeHeaderJSON]}
sendError (ForbiddenError message) =
  return $ err403 {errBody = encode $ ForbiddenError message, errHeaders = [contentTypeHeaderJSON]}
sendError (NotExistsError message) =
  return $ err404 {errBody = encode $ NotExistsError message, errHeaders = [contentTypeHeaderJSON]}
sendError (GeneralServerError message) = do
  logError _CMP_API message
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
