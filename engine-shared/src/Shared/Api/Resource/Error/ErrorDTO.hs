module Shared.Api.Resource.Error.ErrorDTO where

data ErrorDTO
  = AcceptedErrorDTO
  | FoundErrorDTO String
  | ValidationErrorDTO [String] [(String, String)]
  | UserErrorDTO String
  | UnauthorizedErrorDTO String
  | ForbiddenErrorDTO String
  | NotExistsErrorDTO String
  | GeneralServerErrorDTO String
