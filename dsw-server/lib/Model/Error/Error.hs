module Model.Error.Error where

import Model.Localization.LocaleRecord

type FormError = LocaleRecord

type FieldError = (String, LocaleRecord)

data AppError
  = ValidationError [FormError]
                    [FieldError]
  | UserError LocaleRecord
  | UnauthorizedError LocaleRecord
  | ForbiddenError LocaleRecord
  | NotExistsError LocaleRecord
  | GeneralServerError String
  deriving (Show, Eq)
