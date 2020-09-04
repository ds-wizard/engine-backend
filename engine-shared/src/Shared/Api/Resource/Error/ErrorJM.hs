module Shared.Api.Resource.Error.ErrorJM where

import Control.Monad
import Data.Aeson
import Network.HTTP.Types.Status

import Shared.Api.Resource.Error.ErrorDTO

instance ToJSON ErrorDTO where
  toJSON AcceptedErrorDTO = object ["status" .= 202]
  toJSON (FoundErrorDTO _) = object ["status" .= 302]
  toJSON (ValidationErrorDTO formErrors fieldErrors) =
    object ["status" .= 400, "formErrors" .= formErrors, "fieldErrors" .= fieldErrors]
  toJSON (UserErrorDTO errorMessage) = object ["status" .= 400, "message" .= errorMessage]
  toJSON (UnauthorizedErrorDTO errorMessage) = object ["status" .= 401, "message" .= errorMessage]
  toJSON (ForbiddenErrorDTO errorMessage) = object ["status" .= 403, "message" .= errorMessage]
  toJSON (NotExistsErrorDTO errorMessage) = object ["status" .= 404, "message" .= errorMessage]
  toJSON (GeneralServerErrorDTO errorMessage) = object ["status" .= 500]
  toJSON (HttpClientErrorDTO status errorMessage) = object ["statusCode" .= statusCode status, "source" .= "external"]

instance FromJSON ErrorDTO where
  parseJSON (Object o) = do
    status <- o .: "status"
    message <- o .:? "message" .!= ""
    formErrors <- o .:? "formErrors" .!= []
    fieldErrors <- o .:? "fieldErrors" .!= []
    external <- o .:? "source" .!= "internal"
    case (status, message, formErrors, fieldErrors, external) of
      (_, _, _, _, "external") -> return $ HttpClientErrorDTO (toEnum status) message
      (202, _, _, _, _) -> return AcceptedErrorDTO
      (302, _, _, _, _) -> return $ FoundErrorDTO ""
      (400, _, [], [], _) -> return $ UserErrorDTO message
      (400, "", _, _, _) -> return $ ValidationErrorDTO formErrors fieldErrors
      (401, _, _, _, _) -> return $ UnauthorizedErrorDTO message
      (403, _, _, _, _) -> return $ ForbiddenErrorDTO message
      (404, _, _, _, _) -> return $ NotExistsErrorDTO message
      (500, _, _, _, _) -> return $ GeneralServerErrorDTO message
  parseJSON _ = mzero
