module Specs.Common where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO, runReaderT)

import Api.Resource.Error.ErrorDTO
import Localization.Locale
import Model.Context.AppContext
import Model.Localization.LocaleRecord

fakeLogState :: String -> IO ()
fakeLogState _ = return ()

filterJustError :: LogSource -> LogLevel -> Bool
filterJustError _ LevelError = True
filterJustError _ _ = False

runInContext action appContext =
  runStdoutLoggingT . (filterLogger filterJustError) $ runReaderT (runAppContextM action) appContext

runInContextIO action appContext =
  liftIO . runStdoutLoggingT . (filterLogger filterJustError) $ runReaderT (runAppContextM action) appContext

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
