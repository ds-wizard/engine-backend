module Wizard.Service.Config.Client.ClientConfigService where

import Control.Monad (unless)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks)
import Data.Maybe (fromMaybe)

import Shared.Common.Localization.Messages.Public
import Shared.Common.Model.Config.ServerConfig
import Shared.Common.Model.Error.Error
import Wizard.Api.Resource.Config.ClientConfigDTO
import Wizard.Api.Resource.User.UserDTO
import Wizard.Database.DAO.Tenant.Config.TenantConfigSubmissionDAO
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
import WizardLib.Public.Database.DAO.User.UserTourDAO

getClientConfig :: Maybe String -> Maybe String -> AppContextM ClientConfigDTO
getClientConfig mServerUrl mClientUrl = do
  serverConfig <- asks serverConfig
  mCurrentUser <- asks currentUser
  tenant <-
    if serverConfig.cloud.enabled
      then maybe getCurrentTenant findTenantByClientUrl mClientUrl
      else getCurrentTenant
  case tenant.state of
    NotSeededTenantState -> do
      throwError $ UserError (_ERROR_VALIDATION__NOT_SEEDED_TENANT (fromMaybe "not-provided" mServerUrl))
    PendingHousekeepingTenantState -> do
      throwErrorIfTenantIsDisabled mServerUrl tenant
      let mCreatedByUuid = fmap (.uuid) mCurrentUser
      migrateToLatestMetamodelVersionCommand tenant mCreatedByUuid
      return $ HousekeepingInProgressClientConfigDTO {message = "We’re currently upgrading the data to the latest version to enhance your experience"}
    HousekeepingInProgressTenantState -> do
      throwErrorIfTenantIsDisabled mServerUrl tenant
      return $ HousekeepingInProgressClientConfigDTO {message = "We’re currently upgrading the data to the latest version to enhance your experience"}
    ReadyForUseTenantState -> do
      throwErrorIfTenantIsDisabled mServerUrl tenant
      (tenantConfig, tcSubmission) <-
        if serverConfig.cloud.enabled
          then case mClientUrl of
            Just clientUrl -> do
              tenantConfig <- getTenantConfigByUuid tenant.uuid
              tcSubmission <- findTenantConfigSubmissionByTenantUuid tenant.uuid
              return (tenantConfig, tcSubmission)
            Nothing -> do
              tenantConfig <- getCurrentTenantConfig
              tcSubmission <- findTenantConfigSubmission
              return (tenantConfig, tcSubmission)
          else do
            tenantConfig <- getCurrentTenantConfig
            tcSubmission <- findTenantConfigSubmission
            return (tenantConfig, tcSubmission)
      mUserProfile <-
        case mCurrentUser of
          Just currentUser -> do
            userGroupUuids <- findUserGroupUuidsForUserUuidAndTenantUuid currentUser.uuid tenant.uuid
            return . Just $ toUserProfile currentUser userGroupUuids
          Nothing -> return Nothing
      tours <-
        case mCurrentUser of
          Just currentUser -> findUserToursByUserUuid currentUser.uuid
          _ -> return []
      return $ toClientConfigDTO serverConfig tenantConfig tcSubmission mUserProfile tours tenant

throwErrorIfTenantIsDisabled :: Maybe String -> Tenant -> AppContextM ()
throwErrorIfTenantIsDisabled mServerUrl tenant = unless tenant.enabled (throwError . NotExistsError $ _ERROR_VALIDATION__TENANT_OR_ACTIVE_PLAN_ABSENCE (fromMaybe "not-provided" mServerUrl))
