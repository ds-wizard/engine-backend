module Shared.Api.Resource.Error.ErrorJM where

import Data.Aeson

import Shared.Api.Resource.Error.ErrorDTO

instance ToJSON ErrorDTO where
  toJSON AcceptedErrorDTO = object ["status" .= 202]
  toJSON (ValidationErrorDTO formErrors fieldErrors) =
    object ["status" .= 400, "formErrors" .= formErrors, "fieldErrors" .= fieldErrors]
  toJSON (UserErrorDTO errorMessage) = object ["status" .= 400, "message" .= errorMessage]
  toJSON (UnauthorizedErrorDTO errorMessage) = object ["status" .= 401, "message" .= errorMessage]
  toJSON (ForbiddenErrorDTO errorMessage) = object ["status" .= 403, "message" .= errorMessage]
  toJSON (NotExistsErrorDTO errorMessage) = object ["status" .= 404, "message" .= errorMessage]
  toJSON (GeneralServerErrorDTO errorMessage) = object ["status" .= 500, "type" .= "GeneralServerError"]
