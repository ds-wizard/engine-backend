module Shared.Api.Resource.Error.ErrorDTO where

import Network.HTTP.Types.Status

data ErrorDTO
  = AcceptedErrorDTO
  | FoundErrorDTO String
  | ValidationErrorDTO [String] [(String, String)]
  | UserErrorDTO String
  | UnauthorizedErrorDTO String
  | ForbiddenErrorDTO String
  | NotExistsErrorDTO String
  | GeneralServerErrorDTO String
  | HttpClientErrorDTO Status String
  deriving (Show, Eq)
