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
import Wizard.Database.DAO.Tenant.Config.TenantConfigDashboardAndLoginScreenDAO
import Wizard.Database.DAO.Tenant.Config.TenantConfigOrganizationDAO
import Wizard.Database.DAO.Tenant.Config.TenantConfigOwlDAO
import Wizard.Database.DAO.Tenant.Config.TenantConfigPrivacyAndSupportDAO
import Wizard.Database.DAO.Tenant.Config.TenantConfigProjectDAO
import Wizard.Database.DAO.Tenant.Config.TenantConfigSubmissionDAO
import Wizard.Database.DAO.Tenant.TenantDAO
import Wizard.Database.DAO.User.UserGroupMembershipDAO
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Context.AppContext
import Wizard.Model.Tenant.Tenant
import Wizard.Service.Config.Client.ClientConfigMapper
import Wizard.Service.KnowledgeModel.Metamodel.MigrationService
import Wizard.Service.Tenant.Config.ConfigService
import Wizard.Service.Tenant.TenantHelper
import Wizard.Service.User.UserMapper
import WizardLib.Public.Database.DAO.Tenant.Config.TenantConfigFeaturesDAO
import WizardLib.Public.Database.DAO.Tenant.Config.TenantConfigLookAndFeelDAO
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
      (tcOrganization, tcAuthentication, tcPrivacyAndSupport, tcDashboardAndLoginScreen, tcLookAndFeel, tcRegistry, tcProject, tcSubmission, tcFeatures, tcOwl) <-
        case (serverConfig.cloud.enabled, mClientUrl) of
          (True, Just _) -> do
            tcOrganization <- findTenantConfigOrganizationByUuid tenant.uuid
            tcAuthentication <- getTenantConfigAuthenticationByUuid tenant.uuid
            tcPrivacyAndSupport <- findTenantConfigPrivacyAndSupportByUuid tenant.uuid
            tcDashboardAndLoginScreen <- findTenantConfigDashboardAndLoginScreenByUuid tenant.uuid
            tcLookAndFeel <- findTenantConfigLookAndFeelByUuid tenant.uuid
            tcRegistry <- getTenantConfigRegistryByUuid tenant.uuid
            tcProject <- findTenantConfigProjectByUuid tenant.uuid
            tcSubmission <- findTenantConfigSubmissionByUuid tenant.uuid
            tcFeatures <- findTenantConfigFeaturesByUuid tenant.uuid
            tcOwl <- findTenantConfigOwlByUuid tenant.uuid
            return (tcOrganization, tcAuthentication, tcPrivacyAndSupport, tcDashboardAndLoginScreen, tcLookAndFeel, tcRegistry, tcProject, tcSubmission, tcFeatures, tcOwl)
          _ -> do
            tcOrganization <- findTenantConfigOrganization
            tcAuthentication <- getCurrentTenantConfigAuthentication
            tcPrivacyAndSupport <- findTenantConfigPrivacyAndSupport
            tcDashboardAndLoginScreen <- findTenantConfigDashboardAndLoginScreen
            tcLookAndFeel <- findTenantConfigLookAndFeel
            tcRegistry <- getCurrentTenantConfigRegistry
            tcProject <- getCurrentTenantConfigProject
            tcSubmission <- findTenantConfigSubmission
            tcFeatures <- findTenantConfigFeatures
            tcOwl <- findTenantConfigOwl
            return (tcOrganization, tcAuthentication, tcPrivacyAndSupport, tcDashboardAndLoginScreen, tcLookAndFeel, tcRegistry, tcProject, tcSubmission, tcFeatures, tcOwl)
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
      return $ toClientConfigDTO serverConfig tcOrganization tcAuthentication tcPrivacyAndSupport tcDashboardAndLoginScreen tcLookAndFeel tcRegistry tcProject tcSubmission tcFeatures tcOwl mUserProfile tours tenant

throwErrorIfTenantIsDisabled :: Maybe String -> Tenant -> AppContextM ()
throwErrorIfTenantIsDisabled mServerUrl tenant = unless tenant.enabled (throwError . NotExistsError $ _ERROR_VALIDATION__TENANT_OR_ACTIVE_PLAN_ABSENCE (fromMaybe "not-provided" mServerUrl))
