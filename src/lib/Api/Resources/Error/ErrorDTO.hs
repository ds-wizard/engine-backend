module Api.Resources.Error.ErrorDTO where

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
  toJSON (ForbiddenError errorMessage) =
    object ["status" .= 403, "error" .= "Forbidden", "message" .= errorMessage]
  toJSON (NotExistsError errorMessage) =
    object ["status" .= 404, "error" .= "Not Found", "message" .= errorMessage]
  toJSON (DatabaseError errorMessage) =
    object
      [ "status" .= 500
      , "error" .= "Server Internal Error"
      , "type" .= "DatabaseError"
      , "message" .= errorMessage
      ]
