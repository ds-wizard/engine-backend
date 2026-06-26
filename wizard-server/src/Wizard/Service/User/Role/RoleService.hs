module Wizard.Service.User.Role.RoleService where

import Control.Monad (void, when)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks, liftIO)
import Data.Time
import qualified Data.UUID as U

import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.Pageable
import Shared.Common.Model.Common.Sort
import Shared.Common.Model.Error.Error
import Shared.Common.Util.Uuid
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Tenant.Config.TenantConfigAuthenticationDAO
import Wizard.Database.DAO.User.UserDAO
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AclContext
import Wizard.Model.Context.AppContext
import Wizard.Model.Tenant.Config.TenantConfig
import Wizard.Service.User.Role.RoleValidation
import WizardLib.Public.Api.Resource.User.RoleChangeDTO
import WizardLib.Public.Database.DAO.User.RoleDAO
import WizardLib.Public.Model.PersistentCommand.User.CreateOrUpdateRoleCommand
import WizardLib.Public.Model.User.Role
import WizardLib.Public.Model.User.RoleList
import qualified WizardLib.Public.Service.User.RoleMapper as Mapper

getRolesPage :: Maybe String -> Pageable -> [Sort] -> AppContextM (Page RoleList)
getRolesPage mQuery pageable sort = do
  checkPermission _SETTINGS_MANAGE_ROLE_PERMISSION
  findRolesPage mQuery pageable sort

getRole :: U.UUID -> AppContextM RoleList
getRole uuid = do
  checkPermission _SETTINGS_MANAGE_ROLE_PERMISSION
  findRoleListByUuid uuid

createRole :: RoleChangeDTO -> AppContextM RoleList
createRole reqDto =
  runInTransaction $ do
    checkPermission _SETTINGS_MANAGE_ROLE_PERMISSION
    validateRoleChangeDTO reqDto
    uuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    tenantUuid <- asks currentTenantUuid
    let role = Mapper.fromCreateDTO reqDto uuid tenantUuid now
    insertRole role
    findRoleListByUuid uuid

modifyRole :: U.UUID -> RoleChangeDTO -> AppContextM RoleList
modifyRole uuid reqDto =
  runInTransaction $ do
    checkPermission _SETTINGS_MANAGE_ROLE_PERMISSION
    validateRoleChangeDTO reqDto
    role <- findRoleByUuid uuid
    when role.isAdmin (throwError $ UserError _ERROR_VALIDATION__USER_ROLE_ADMIN_CANNOT_BE_CHANGED)
    now <- liftIO getCurrentTime
    let updated = Mapper.fromChangeDTO role reqDto now
    updateRoleByUuid updated
    void $ updateUsersRoleByRole uuid reqDto.permissions reqDto.name
    findRoleListByUuid uuid

deleteRole :: U.UUID -> AppContextM ()
deleteRole uuid =
  runInTransaction $ do
    checkPermission _SETTINGS_MANAGE_ROLE_PERMISSION
    role <- findRoleByUuid uuid
    when role.isAdmin (throwError $ UserError _ERROR_VALIDATION__USER_ROLE_ADMIN_CANNOT_BE_DELETED)
    tcAuthentication <- findTenantConfigAuthentication
    when (tcAuthentication.defaultRoleUuid == uuid) (throwError $ UserError _ERROR_VALIDATION__USER_ROLE_IS_DEFAULT)
    count <- countUsersByRole uuid
    when (count > 0) (throwError $ UserError _ERROR_VALIDATION__USER_ROLE_IN_USE)
    void $ deleteRoleByUuid uuid

createOrUpdateRoleFromCommand :: CreateOrUpdateRoleCommand -> AppContextM ()
createOrUpdateRoleFromCommand command =
  runInTransaction $ do
    now <- liftIO getCurrentTime
    mRole <- findRoleByUuid' command.uuid
    case mRole of
      Just role -> void $ updateRoleByUuid (Mapper.fromCommandUpdate role command now)
      Nothing -> do
        tenantUuid <- asks currentTenantUuid
        void $ insertRole (Mapper.fromCommandCreate command tenantUuid now)
    void $ updateUsersRoleByRole command.uuid command.permissions command.name

deleteRoleFromCommand :: U.UUID -> AppContextM ()
deleteRoleFromCommand uuid =
  runInTransaction $ void $ deleteRoleByUuid uuid
