module Wizard.Service.User.UserValidation where

import Control.Monad (when)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks)
import Data.Char (toLower)
import qualified Data.Map.Strict as M
import qualified Data.UUID as U

import Shared.Common.Model.Error.Error
import Wizard.Database.DAO.User.UserDAO
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext

validateUserEmailUniqueness :: String -> U.UUID -> AppContextM ()
validateUserEmailUniqueness email tenantUuid = do
  mUserFromDb <- findUserByEmailAndTenantUuid' (toLower <$> email) tenantUuid
  case mUserFromDb of
    Just _ -> throwError $ ValidationError [] (M.singleton "email" [_ERROR_VALIDATION__USER_EMAIL_UNIQUENESS email])
    Nothing -> return ()

validateUserChangedEmailUniqueness :: String -> String -> AppContextM ()
validateUserChangedEmailUniqueness newEmail oldEmail = do
  tenantUuid <- asks currentTenantUuid
  when (newEmail /= oldEmail) (validateUserEmailUniqueness newEmail tenantUuid)
