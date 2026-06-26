module Wizard.Service.User.Role.RoleValidation where

import Control.Monad (forM_, unless, when)
import Control.Monad.Except (throwError)

import Shared.Common.Model.Error.Error
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext
import WizardLib.Public.Api.Resource.User.RoleChangeDTO
import WizardLib.Public.Model.User.RolePermission

validateRoleChangeDTO :: RoleChangeDTO -> AppContextM ()
validateRoleChangeDTO dto = do
  when (null dto.name) (throwError $ UserError _ERROR_VALIDATION__USER_ROLE_NAME_EMPTY)
  forM_ dto.permissions $ \permission ->
    unless (permission `elem` assignableRolePermissions) (throwError . UserError $ _ERROR_VALIDATION__USER_ROLE_INVALID_PERMISSION permission)
