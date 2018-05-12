module Service.User.UserValidation where

import Data.Either (isRight)

import Common.Context
import Common.Error
import Common.Localization
import Common.Types
import Database.DAO.User.UserDAO

validateUserEmailUniqueness :: Context -> Email -> IO (Maybe AppError)
validateUserEmailUniqueness context email = do
  eitherUserFromDb <- findUserByEmail context email
  if isRight eitherUserFromDb
    then return . Just . createErrorWithFieldError $ ("email", _ERROR_VALIDATION__USER_EMAIL_UNIQUENESS email)
    else return Nothing

validateUserChangedEmailUniqueness :: Context -> Email -> Email -> IO (Maybe AppError)
validateUserChangedEmailUniqueness context newEmail oldEmail = do
  if newEmail /= oldEmail
    then validateUserEmailUniqueness context newEmail
    else return Nothing

-- --------------------------------
-- HELPERS
-- --------------------------------
heValidateUserEmailUniqueness context email callback = do
  maybeError <- validateUserEmailUniqueness context email
  case maybeError of
    Nothing -> callback
    Just error -> return . Left $ error

heValidateUserChangedEmailUniqueness context newEmail oldEmail callback = do
  maybeError <- validateUserChangedEmailUniqueness context newEmail oldEmail
  case maybeError of
    Nothing -> callback
    Just error -> return . Left $ error
