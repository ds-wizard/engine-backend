module Wizard.Service.User.UserValidation where

import Data.Char (toLower)
import Data.Either (isRight)

import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import Wizard.Database.DAO.User.UserDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.User.User

validateUserEmailUniqueness :: Email -> AppContextM (Maybe AppError)
validateUserEmailUniqueness email = do
  eitherUserFromDb <- findUserByEmail (toLower <$> email)
  if isRight eitherUserFromDb
    then return . Just $ ValidationError [] [("email", _ERROR_VALIDATION__USER_EMAIL_UNIQUENESS email)]
    else return Nothing

validateUserChangedEmailUniqueness :: Email -> Email -> AppContextM (Maybe AppError)
validateUserChangedEmailUniqueness newEmail oldEmail =
  if newEmail /= oldEmail
    then validateUserEmailUniqueness newEmail
    else return Nothing

-- --------------------------------
-- HELPERS
-- --------------------------------
heValidateUserEmailUniqueness email callback = do
  maybeError <- validateUserEmailUniqueness email
  case maybeError of
    Nothing -> callback
    Just error -> return . Left $ error

heValidateUserChangedEmailUniqueness newEmail oldEmail callback = do
  maybeError <- validateUserChangedEmailUniqueness newEmail oldEmail
  case maybeError of
    Nothing -> callback
    Just error -> return . Left $ error
