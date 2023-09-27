module Wizard.Service.UserToken.Login.LoginValidation where

import Control.Monad (when)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (liftIO)
import Data.Time
import qualified Data.UUID as U

import Shared.ActionKey.Database.DAO.ActionKey.ActionKeyDAO
import Shared.ActionKey.Model.ActionKey.ActionKey
import Shared.Common.Model.Error.Error
import Wizard.Database.Mapping.ActionKey.ActionKeyType ()
import Wizard.Model.ActionKey.ActionKeyType
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.Tenant.Config.TenantConfig
import Wizard.Model.User.User
import Wizard.Service.User.UserUtil
import WizardLib.Public.Api.Resource.UserToken.LoginDTO
import WizardLib.Public.Localization.Messages.Public

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

validateCode :: User -> Int -> TenantConfig -> AppContextM ()
validateCode user code tenantConfig = do
  mActionKey <- findActionKeyByIdentityAndHash' (U.toString user.uuid) (show code) :: AppContextM (Maybe (ActionKey U.UUID ActionKeyType))
  case mActionKey of
    Just actionKey -> do
      let timeDelta = realToFrac . toInteger $ tenantConfig.authentication.internal.twoFactorAuth.expiration
      now <- liftIO getCurrentTime
      when (addUTCTime timeDelta actionKey.createdAt < now) (throwError $ UserError _ERROR_SERVICE_TOKEN__CODE_IS_EXPIRED)
    Nothing -> throwError $ UserError _ERROR_SERVICE_TOKEN__INCORRECT_CODE
