module Api.Resource.Error.ErrorDTO where

import Control.Monad
import Data.Aeson

import Common.Error

instance ToJSON AppError where
  toJSON (ValidationError errorMessage formErrors fieldErrors) =
    object
      [ "status" .= 400
      , "error" .= "Bad Request"
      , "message" .= errorMessage
      , "formErrors" .= formErrors
      , "fieldErrors" .= fieldErrors
      ]
  toJSON (NotExistsError errorMessage) = object ["status" .= 404, "error" .= "Not Found", "message" .= errorMessage]
  toJSON (DatabaseError errorMessage) =
    object ["status" .= 500, "error" .= "Server Internal Error", "type" .= "DatabaseError", "message" .= errorMessage]
  toJSON (MigratorError errorMessage) =
    object ["status" .= 400, "error" .= "Bad Request", "type" .= "MigratorError", "message" .= errorMessage]
  toJSON (HttpClientError errorMessage) =
    object ["status" .= 500, "error" .= "Internal Server Error", "type" .= "HttpClientError", "message" .= errorMessage]
  toJSON (GeneralServerError errorMessage) =
    object
      ["status" .= 500, "error" .= "Internal Server Error", "type" .= "GeneralServerError", "message" .= errorMessage]

instance FromJSON AppError where
  parseJSON (Object o) = do
    errorType <- o .: "errorType"
    case errorType of
      "ValidationError" -> do
        message <- o .: "message"
        formErrors <- o .: "formErrors"
        fieldErrors <- o .: "fieldErrors"
        return $ ValidationError message formErrors fieldErrors
      "NotExistsError" -> do
        message <- o .: "message"
        return $ NotExistsError message
      "DatabaseError" -> do
        message <- o .: "message"
        return $ DatabaseError message
      "MigratorError" -> do
        message <- o .: "message"
        return $ MigratorError message
      "HttpClientError" -> do
        message <- o .: "message"
        return $ HttpClientError message
      "GeneralServerError" -> do
        message <- o .: "message"
        return $ GeneralServerError message
  parseJSON _ = mzero
