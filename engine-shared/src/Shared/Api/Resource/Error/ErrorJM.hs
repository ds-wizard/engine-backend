module Shared.Api.Resource.Error.ErrorJM where

import Control.Monad
import Data.Aeson
import Network.HTTP.Types.Status

import Shared.Api.Resource.Localization.LocaleRecordJM ()
import Shared.Model.Error.Error

instance ToJSON AppError where
  toJSON AcceptedError = object ["status" .= 202]
  toJSON (FoundError _) = object ["status" .= 302]
  toJSON (UserError error) = object ["status" .= 400, "type" .= "UserSimpleError", "error" .= error]
  toJSON (SystemLogError error) = object ["status" .= 400, "type" .= "SystemLogError", "error" .= error]
  toJSON (ValidationError formErrors fieldErrors) =
    object ["status" .= 400, "type" .= "UserFormError", "formErrors" .= formErrors, "fieldErrors" .= fieldErrors]
  toJSON (UnauthorizedError message) = object ["status" .= 401, "message" .= message]
  toJSON (ForbiddenError message) = object ["status" .= 403, "message" .= message]
  toJSON (NotExistsError message) = object ["status" .= 404, "message" .= message]
  toJSON LockedError = object ["status" .= 423]
  toJSON (GeneralServerError _) = object ["status" .= 500]
  toJSON (HttpClientError status _) = object ["statusCode" .= statusCode status, "source" .= "external"]

instance FromJSON AppError where
  parseJSON (Object o) = do
    status <- o .: "status"
    external <- o .:? "source" .!= "internal"
    userErrorType <- o .:? "type" .!= "UserSimpleError"
    case (status, userErrorType, external) of
      (_, _, "external") -> return $ HttpClientError (toEnum status) ""
      (202, _, _) -> return AcceptedError
      (302, _, _) -> return $ FoundError ""
      (400, "UserSimpleError", _) -> do
        error <- o .: "error"
        return . UserError $ error
      (400, "SystemLogError", _) -> do
        error <- o .: "error"
        return . SystemLogError $ error
      (400, "UserFormError", _) -> do
        formErrors <- o .: "formErrors"
        fieldErrors <- o .: "fieldErrors"
        return $ ValidationError formErrors fieldErrors
      (401, _, _) -> do
        message <- o .: "message"
        return . UnauthorizedError $ message
      (403, _, _) -> do
        message <- o .: "message"
        return . ForbiddenError $ message
      (404, _, _) -> do
        message <- o .: "message"
        return . NotExistsError $ message
      (423, _, _) -> return LockedError
      (500, _, _) -> return $ GeneralServerError ""
  parseJSON _ = mzero
