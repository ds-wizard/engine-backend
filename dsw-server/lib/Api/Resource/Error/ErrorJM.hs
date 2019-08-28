module Api.Resource.Error.ErrorJM where

import Data.Aeson

import Api.Resource.Error.ErrorDTO

instance ToJSON ErrorDTO where
  toJSON (ValidationErrorDTO formErrors fieldErrors) =
    object ["status" .= 400, "error" .= "Bad Request", "formErrors" .= formErrors, "fieldErrors" .= fieldErrors]
  toJSON (UserErrorDTO errorMessage) = object ["status" .= 400, "error" .= "Bad Request", "message" .= errorMessage]
  toJSON (UnauthorizedErrorDTO errorMessage) =
    object ["status" .= 401, "error" .= "Unauthorized", "message" .= errorMessage]
  toJSON (ForbiddenErrorDTO errorMessage) = object ["status" .= 403, "error" .= "Forbidden", "message" .= errorMessage]
  toJSON (NotExistsErrorDTO errorMessage) = object ["status" .= 404, "error" .= "Not Found", "message" .= errorMessage]
  toJSON (GeneralServerErrorDTO errorMessage) =
    object ["status" .= 500, "error" .= "Internal Server Error", "type" .= "GeneralServerError"]
