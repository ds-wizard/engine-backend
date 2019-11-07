module Model.Error.Error where

type ErrorMessage = String

type FormError = String

type FieldError = (String, String)

data AppError
  = ValidationError ErrorMessage
                    [FormError]
                    [FieldError]
  | NotExistsError ErrorMessage
  | DatabaseError ErrorMessage
  | MigratorError ErrorMessage
  | HttpClientError ErrorMessage
  | ForbiddenError ErrorMessage
  | GeneralServerError ErrorMessage
  deriving (Show, Eq)
