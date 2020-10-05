module SharedTest.Specs.Common where

import Control.Monad.Logger

import Shared.Api.Resource.Error.ErrorDTO
import Shared.Model.Localization.LocaleRecord
import Shared.Util.String

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
    locale (LocaleRecord _ defaultMessage variables) = f' defaultMessage variables
    localeTuple (key, LocaleRecord _ defaultMessage variables) = (key, f' defaultMessage variables)

createUserError :: LocaleRecord -> ErrorDTO
createUserError (LocaleRecord _ defaultMessage variables) = UserErrorDTO (f' defaultMessage variables)

createUnauthorizedError :: LocaleRecord -> ErrorDTO
createUnauthorizedError (LocaleRecord _ defaultMessage variables) = UnauthorizedErrorDTO (f' defaultMessage variables)

createForbiddenError :: LocaleRecord -> ErrorDTO
createForbiddenError (LocaleRecord _ defaultMessage variables) = ForbiddenErrorDTO (f' defaultMessage variables)

createNotExistsError :: LocaleRecord -> ErrorDTO
createNotExistsError (LocaleRecord _ defaultMessage variables) = NotExistsErrorDTO (f' defaultMessage variables)
