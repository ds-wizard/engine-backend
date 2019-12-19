module SharedTest.Specs.Common where

import Control.Monad.Logger

import Shared.Api.Resource.Error.ErrorDTO
import Shared.Localization.Locale
import Shared.Model.Localization.LocaleRecord

fakeLogState :: String -> IO ()
fakeLogState _ = return ()

filterJustError :: LogSource -> LogLevel -> Bool
filterJustError _ LevelError = True
filterJustError _ _ = False

createValidationError :: [LocaleRecord] -> [(String, LocaleRecord)] -> ErrorDTO
createValidationError formErrorRecords fieldErrorRecords =
  let formErrors = fmap locale formErrorRecords
      fieldErrors = fmap localeTuple fieldErrorRecords
   in ValidationErrorDTO formErrors fieldErrors
  where
    locale (LocaleRecord _ defaultMessage variables) = format defaultMessage variables
    localeTuple (key, LocaleRecord _ defaultMessage variables) = (key, format defaultMessage variables)

createUserError :: LocaleRecord -> ErrorDTO
createUserError (LocaleRecord _ defaultMessage variables) = UserErrorDTO (format defaultMessage variables)

createUnauthorizedError :: LocaleRecord -> ErrorDTO
createUnauthorizedError (LocaleRecord _ defaultMessage variables) =
  UnauthorizedErrorDTO (format defaultMessage variables)

createForbiddenError :: LocaleRecord -> ErrorDTO
createForbiddenError (LocaleRecord _ defaultMessage variables) = ForbiddenErrorDTO (format defaultMessage variables)

createNotExistsError :: LocaleRecord -> ErrorDTO
createNotExistsError (LocaleRecord _ defaultMessage variables) = NotExistsErrorDTO (format defaultMessage variables)
