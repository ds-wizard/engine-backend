module Wizard.Service.UserToken.Login.LoginValidation where

import Control.Monad (when)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (liftIO)
import Data.Time

import Shared.Common.Model.Error.Error
import Wizard.Api.Resource.UserToken.LoginDTO
import Wizard.Database.DAO.ActionKey.ActionKeyDAO
import Wizard.Localization.Messages.Public
import Wizard.Model.ActionKey.ActionKey
import Wizard.Model.Config.AppConfig
import Wizard.Model.Context.AppContext
import Wizard.Model.User.User
import Wizard.Service.User.UserUtil

validate :: LoginDTO -> User -> AppContextM ()
validate reqDto user = do
  validateIsUserActive user
  validateUserPassword reqDto user

validateIsUserActive :: User -> AppContextM ()
validateIsUserActive user =
  if user.active
    then return ()
    else throwError $ UserError _ERROR_SERVICE_TOKEN__ACCOUNT_IS_NOT_ACTIVATED

validateUserPassword :: LoginDTO -> User -> AppContextM ()
validateUserPassword reqDto user =
  if verifyPassword reqDto.password user.passwordHash
    then return ()
    else throwError $ UserError _ERROR_SERVICE_TOKEN__INCORRECT_EMAIL_OR_PASSWORD

validateCode :: User -> Int -> AppConfig -> AppContextM ()
validateCode user code appConfig = do
  mActionKey <- findActionKeyByUserIdAndHash' user.uuid (show code)
  case mActionKey of
    Just actionKey -> do
      let timeDelta = realToFrac . toInteger $ appConfig.authentication.internal.twoFactorAuth.expiration
      now <- liftIO getCurrentTime
      when (addUTCTime timeDelta actionKey.createdAt < now) (throwError $ UserError _ERROR_SERVICE_TOKEN__CODE_IS_EXPIRED)
    Nothing -> throwError $ UserError _ERROR_SERVICE_TOKEN__INCORRECT_CODE
