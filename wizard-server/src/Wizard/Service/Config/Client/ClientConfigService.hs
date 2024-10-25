module Wizard.Service.Config.Client.ClientConfigService where

import Control.Monad (unless)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks)

import Shared.Common.Model.Config.ServerConfig
import Shared.Common.Model.Error.Error
import Wizard.Api.Resource.Config.ClientConfigDTO
import Wizard.Api.Resource.User.UserDTO
import Wizard.Database.DAO.Locale.LocaleDAO
import Wizard.Database.DAO.Tenant.TenantDAO
import Wizard.Database.DAO.User.UserGroupMembershipDAO
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Context.AppContext
import Wizard.Model.Tenant.Tenant
import Wizard.Service.Config.Client.ClientConfigMapper
import Wizard.Service.Migration.Metamodel.MigratorService
import Wizard.Service.Tenant.Config.ConfigService
import Wizard.Service.Tenant.TenantHelper
import Wizard.Service.User.UserMapper

getClientConfig :: Maybe String -> AppContextM ClientConfigDTO
getClientConfig mClientUrl = do
  serverConfig <- asks serverConfig
  mCurrentUser <- asks currentUser
  tenant <-
    if serverConfig.cloud.enabled
      then maybe getCurrentTenant findTenantByClientUrl mClientUrl
      else getCurrentTenant
  unless tenant.enabled (throwError LockedError)
  case tenant.state of
    PendingHousekeepingTenantState -> do
      let mCreatedByUuid = fmap (.uuid) mCurrentUser
      migrateToLatestMetamodelVersionCommand tenant mCreatedByUuid
      return $ HousekeepingInProgressClientConfigDTO {message = "We’re currently upgrading the data to the latest version to enhance your experience"}
    HousekeepingInProgressTenantState -> do
      return $ HousekeepingInProgressClientConfigDTO {message = "We’re currently upgrading the data to the latest version to enhance your experience"}
    ReadyForUseTenantState -> do
      tenantConfig <-
        if serverConfig.cloud.enabled
          then case mClientUrl of
            Just clientUrl -> getTenantConfigByUuid tenant.uuid
            Nothing -> getCurrentTenantConfig
          else getCurrentTenantConfig
      locales <- findLocalesFilteredWithTenant tenant.uuid [("enabled", show True)]
      mUserProfile <-
        case mCurrentUser of
          Just currentUser -> do
            userGroupUuids <- findUserGroupUuidsForUserUuidAndTenantUuid currentUser.uuid tenant.uuid
            return . Just $ toUserProfile currentUser userGroupUuids
          Nothing -> return Nothing
      return $ toClientConfigDTO serverConfig tenantConfig mUserProfile tenant locales
