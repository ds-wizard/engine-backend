module Wizard.Service.User.UserValidation where

import Control.Monad (when)
import Control.Monad.Except (throwError)
import Data.Char (toLower)

import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import Wizard.Database.DAO.User.UserDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.User.User

validateUserEmailUniqueness :: Email -> AppContextM ()
validateUserEmailUniqueness email = do
  mUserFromDb <- findUserByEmail' (toLower <$> email)
  case mUserFromDb of
    Just _ -> throwError $ ValidationError [] [("email", _ERROR_VALIDATION__USER_EMAIL_UNIQUENESS email)]
    Nothing -> return ()

validateUserChangedEmailUniqueness :: Email -> Email -> AppContextM ()
validateUserChangedEmailUniqueness newEmail oldEmail =
  when (newEmail /= oldEmail) (validateUserEmailUniqueness newEmail)
