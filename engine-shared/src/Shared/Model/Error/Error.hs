module Shared.Model.Error.Error where

import Shared.Model.Localization.LocaleRecord

type FormError = LocaleRecord

type FieldError = (String, LocaleRecord)

data AppError
  = AcceptedError
  | ValidationError [FormError] [FieldError]
  | UserError LocaleRecord
  | UnauthorizedError LocaleRecord
  | ForbiddenError LocaleRecord
  | NotExistsError LocaleRecord
  | GeneralServerError String
  deriving (Show, Eq)
