module Shared.Api.Resource.Error.ErrorDTO where

data ErrorDTO
  = AcceptedErrorDTO
  | ValidationErrorDTO [String] [(String, String)]
  | UserErrorDTO String
  | UnauthorizedErrorDTO String
  | ForbiddenErrorDTO String
  | NotExistsErrorDTO String
  | GeneralServerErrorDTO String
