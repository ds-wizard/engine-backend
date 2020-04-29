module Shared.Model.Error.Error where

import Network.HTTP.Types.Status

import Shared.Model.Localization.LocaleRecord

type FormError = LocaleRecord

type FieldError = (String, LocaleRecord)

data AppError
  = AcceptedError
  | FoundError String
  | ValidationError [FormError] [FieldError]
  | UserError LocaleRecord
  | UnauthorizedError LocaleRecord
  | ForbiddenError LocaleRecord
  | NotExistsError LocaleRecord
  | GeneralServerError String
  | HttpClientError Status String
  deriving (Show, Eq)
