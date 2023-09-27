module Wizard.Service.PersistentCommand.PersistentCommandUtil where

import qualified Data.UUID as U

import Shared.PersistentCommand.Model.PersistentCommand.PersistentCommand
import Wizard.Api.Resource.PersistentCommand.PersistentCommandDTO
import Wizard.Database.DAO.Tenant.TenantDAO
import Wizard.Database.DAO.User.UserDAO
import Wizard.Model.Context.AppContext
import Wizard.Service.PersistentCommand.PersistentCommandMapper
import Wizard.Service.Tenant.TenantUtil

enhancePersistentCommand :: PersistentCommand U.UUID -> AppContextM PersistentCommandDTO
enhancePersistentCommand command = do
  mUser <-
    case command.createdBy of
      Just userUuid -> findUserByUuidSystem' userUuid
      Nothing -> return Nothing
  tenant <- findTenantByUuid command.tenantUuid
  tenantDto <- enhanceTenant tenant
  return $ toDTO command mUser tenantDto
