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
