module Wizard.Service.UserToken.Login.LoginValidation where

import Control.Monad (when)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (liftIO)
import Data.Time
import qualified Data.UUID as U

import Shared.Common.Model.Error.Error
import Shared.UserEmailLink.Database.DAO.UserEmailLink.UserEmailLinkDAO
import Shared.UserEmailLink.Model.UserEmailLink.UserEmailLink
import Wizard.Database.Mapping.UserEmailLink.UserEmailLinkType ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.Tenant.Config.TenantConfig
import Wizard.Model.User.User
import Wizard.Model.UserEmailLink.UserEmailLinkType
import Wizard.Service.User.UserUtil
import WizardLib.Public.Api.Resource.UserToken.LoginDTO
import WizardLib.Public.Database.DAO.User.RoleDAO
import WizardLib.Public.Localization.Messages.Public
import WizardLib.Public.Model.User.Role

validate :: LoginDTO -> User -> AppContextM ()
validate reqDto user = do
  validateIsUserActive user
  validateUserPassword reqDto user

validateLoginEnabled :: TenantConfigAuthentication -> User -> AppContextM ()
validateLoginEnabled tcInternalAuthentication user = do
  role <- findRoleByUuid user.role.uuid
  when (not tcInternalAuthentication.internal.nonAdminLoginEnabled && not role.isAdmin) $
    throwError . UserError $
      _ERROR_SERVICE_TOKEN__INCORRECT_EMAIL_OR_PASSWORD

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

validateCode :: User -> Int -> TenantConfigAuthentication -> AppContextM ()
validateCode user code tcAuthentication = do
  mUserEmailLink <- findUserEmailLinkByIdentityAndHash' (U.toString user.uuid) (show code) :: AppContextM (Maybe (UserEmailLink U.UUID UserEmailLinkType))
  case mUserEmailLink of
    Just userEmailLink -> do
      let timeDelta = realToFrac . toInteger $ tcAuthentication.internal.twoFactorAuth.expiration
      now <- liftIO getCurrentTime
      when (addUTCTime timeDelta userEmailLink.createdAt < now) (throwError $ UserError _ERROR_SERVICE_TOKEN__CODE_IS_EXPIRED)
    Nothing -> throwError $ UserError _ERROR_SERVICE_TOKEN__INCORRECT_CODE
