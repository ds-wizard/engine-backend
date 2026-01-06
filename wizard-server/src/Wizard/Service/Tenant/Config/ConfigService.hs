module Wizard.Service.Tenant.Config.ConfigService where

import Control.Monad.Reader (asks, liftIO)
import Data.Time
import qualified Data.UUID as U

import Shared.Common.Model.Common.SensitiveData
import Wizard.Api.Resource.Tenant.Config.TenantConfigChangeDTO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Tenant.Config.TenantConfigAuthenticationDAO
import Wizard.Database.DAO.Tenant.Config.TenantConfigDashboardAndLoginScreenDAO
import Wizard.Database.DAO.Tenant.Config.TenantConfigKnowledgeModelDAO
import Wizard.Database.DAO.Tenant.Config.TenantConfigOrganizationDAO
import Wizard.Database.DAO.Tenant.Config.TenantConfigOwlDAO
import Wizard.Database.DAO.Tenant.Config.TenantConfigPrivacyAndSupportDAO
import Wizard.Database.DAO.Tenant.Config.TenantConfigProjectDAO
import Wizard.Database.DAO.Tenant.Config.TenantConfigRegistryDAO
import Wizard.Database.DAO.Tenant.Config.TenantConfigSubmissionDAO
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Context.AclContext
import Wizard.Model.Context.AppContext
import Wizard.Model.Tenant.Config.TenantConfig
import Wizard.Model.Tenant.Config.TenantConfigEM ()
import Wizard.Service.Tenant.Config.ConfigMapper
import Wizard.Service.Tenant.Config.ConfigValidation
import WizardLib.Public.Database.DAO.Tenant.Config.TenantConfigFeaturesDAO
import WizardLib.Public.Database.DAO.Tenant.Config.TenantConfigLookAndFeelDAO
import WizardLib.Public.Model.Tenant.Config.TenantConfig
import WizardLib.Public.Service.Tenant.Config.ConfigMapper

getCurrentTenantConfigDto :: AppContextM TenantConfig
getCurrentTenantConfigDto = do
  checkPermission _CFG_PERM
  tcOrganization <- findTenantConfigOrganization
  tcAuthentication <- getCurrentTenantConfigAuthentication
  tcPrivacyAndSupport <- findTenantConfigPrivacyAndSupport
  tcDashboardAndLoginScreen <- findTenantConfigDashboardAndLoginScreen
  tcLookAndFeel <- findTenantConfigLookAndFeel
  tcRegistry <- getCurrentTenantConfigRegistry
  tcKnowledgeModel <- getCurrentTenantConfigKnowledgeModel
  tcProject <- getCurrentTenantConfigProject
  tcSubmission <- findTenantConfigSubmission
  tcFeatures <- findTenantConfigFeatures
  tcOwl <- findTenantConfigOwl
  return $ toTenantConfig tcOrganization tcAuthentication tcPrivacyAndSupport tcDashboardAndLoginScreen tcLookAndFeel tcRegistry tcKnowledgeModel tcProject tcSubmission tcFeatures tcOwl

modifyTenantConfigDto :: TenantConfigChangeDTO -> AppContextM TenantConfig
modifyTenantConfigDto reqDto =
  runInTransaction $ do
    checkPermission _CFG_PERM
    validateTenantConfig reqDto
    now <- liftIO getCurrentTime
    -- Organization
    tcOrganization <- findTenantConfigOrganization
    let tcOrganizationUpdated = fromOrganizationChangeDTO reqDto.organization tcOrganization.tenantUuid tcOrganization.createdAt now
    updateTenantConfigOrganization tcOrganizationUpdated
    -- Authentication
    tcAuthentication <- getCurrentTenantConfigAuthentication
    let tcAuthenticationUpdated = fromAuthenticationChangeDTO reqDto.authentication tcAuthentication.tenantUuid tcAuthentication.createdAt now
    modifyTenantConfigAuthentication tcAuthenticationUpdated
    -- PrivacyAndSupport
    tcPrivacyAndSupport <- findTenantConfigPrivacyAndSupport
    let tcPrivacyAndSupportUpdated = fromPrivacyAndSupportChangeDTO reqDto.privacyAndSupport tcPrivacyAndSupport.tenantUuid tcPrivacyAndSupport.createdAt now
    updateTenantConfigPrivacyAndSupport tcPrivacyAndSupportUpdated
    -- DashboardAndLoginScreen
    tcDashboardAndLoginScreen <- findTenantConfigLookAndFeel
    let tcDashboardAndLoginScreenUpdated = fromDashboardAndLoginScreenChangeDTO reqDto.dashboardAndLoginScreen tcDashboardAndLoginScreen.tenantUuid tcDashboardAndLoginScreen.createdAt now
    updateTenantConfigDashboardAndLoginScreen tcDashboardAndLoginScreenUpdated
    -- LookAndFeel
    tcLookAndFeel <- findTenantConfigLookAndFeel
    let tcLookAndFeelUpdated = fromLookAndFeelChangeDTO reqDto.lookAndFeel tcLookAndFeel.tenantUuid tcLookAndFeel.createdAt now
    updateTenantConfigLookAndFeel tcLookAndFeelUpdated
    -- Registry
    tcRegistry <- getCurrentTenantConfigRegistry
    let tcRegistryUpdated = fromRegistryChangeDTO reqDto.registry tcRegistry.tenantUuid tcRegistry.createdAt now
    modifyTenantConfigRegistry tcRegistryUpdated
    -- KnowledgeModel
    tcKnowledgeModelUpdated <- getCurrentTenantConfigKnowledgeModel
    let tcKnowledgeModelUpdatedUpdated = fromKnowledgeModelChangeDTO reqDto.knowledgeModel tcKnowledgeModelUpdated.tenantUuid tcKnowledgeModelUpdated.createdAt now
    modifyTenantConfigKnowledgeModel tcKnowledgeModelUpdatedUpdated
    -- Project
    tcProject <- getCurrentTenantConfigProject
    let tcProjectUpdated = fromProjectChangeDTO reqDto.project tcProject.tenantUuid tcProject.createdAt now
    modifyTenantConfigProject tcProjectUpdated
    -- Submission
    tcSubmission <- findTenantConfigSubmission
    let tcSubmissionUpdated = fromSubmissionChangeDTO reqDto.submission tcSubmission.tenantUuid tcSubmission.createdAt now
    updateTenantConfigSubmission tcSubmissionUpdated
    -- Features
    tcFeatures <- findTenantConfigFeatures
    let tcFeaturesUpdated = fromFeaturesChangeDTO reqDto.features tcFeatures tcFeatures.tenantUuid tcFeatures.createdAt tcFeatures.updatedAt
    updateTenantConfigFeatures tcFeaturesUpdated
    -- Owl
    tcOwl <- findTenantConfigOwl
    return $ toTenantConfig tcOrganizationUpdated tcAuthenticationUpdated tcPrivacyAndSupportUpdated tcDashboardAndLoginScreenUpdated tcLookAndFeelUpdated tcRegistryUpdated tcKnowledgeModelUpdated tcProjectUpdated tcSubmissionUpdated tcFeaturesUpdated tcOwl

getCurrentTenantConfigAuthentication :: AppContextM TenantConfigAuthentication
getCurrentTenantConfigAuthentication = do
  serverConfig <- asks serverConfig
  encryptedTcAuthentication <- findTenantConfigAuthentication
  return $ process serverConfig.general.secret encryptedTcAuthentication

getTenantConfigAuthenticationByUuid :: U.UUID -> AppContextM TenantConfigAuthentication
getTenantConfigAuthenticationByUuid tenantUuid = do
  serverConfig <- asks serverConfig
  encryptedTcAuthentication <- findTenantConfigAuthenticationByUuid tenantUuid
  return $ process serverConfig.general.secret encryptedTcAuthentication

modifyTenantConfigAuthentication :: TenantConfigAuthentication -> AppContextM TenantConfigAuthentication
modifyTenantConfigAuthentication tcAuthentication =
  runInTransaction $ do
    serverConfig <- asks serverConfig
    let encryptedUpdatedTcAuthentication = process serverConfig.general.secret tcAuthentication
    updateTenantConfigAuthentication encryptedUpdatedTcAuthentication
    return tcAuthentication

getCurrentTenantConfigRegistry :: AppContextM TenantConfigRegistry
getCurrentTenantConfigRegistry = do
  serverConfig <- asks serverConfig
  encryptedTcRegistry <- findTenantConfigRegistry
  return $ process serverConfig.general.secret encryptedTcRegistry

getTenantConfigRegistryByUuid :: U.UUID -> AppContextM TenantConfigRegistry
getTenantConfigRegistryByUuid tenantUuid = do
  serverConfig <- asks serverConfig
  encryptedTcRegistry <- findTenantConfigRegistryByUuid tenantUuid
  return $ process serverConfig.general.secret encryptedTcRegistry

modifyTenantConfigRegistry :: TenantConfigRegistry -> AppContextM TenantConfigRegistry
modifyTenantConfigRegistry tcRegistry =
  runInTransaction $ do
    serverConfig <- asks serverConfig
    let encryptedUpdatedTcRegistry = process serverConfig.general.secret tcRegistry
    updateTenantConfigRegistry encryptedUpdatedTcRegistry
    return tcRegistry

getCurrentTenantConfigKnowledgeModel :: AppContextM TenantConfigKnowledgeModel
getCurrentTenantConfigKnowledgeModel = do
  serverConfig <- asks serverConfig
  encryptedTcKnowledgeModel <- findTenantConfigKnowledgeModel
  return $ process serverConfig.general.secret encryptedTcKnowledgeModel

getTenantConfigKnowledgeModelByUuid :: U.UUID -> AppContextM TenantConfigKnowledgeModel
getTenantConfigKnowledgeModelByUuid tenantUuid = do
  serverConfig <- asks serverConfig
  encryptedTcKnowledgeModel <- findTenantConfigKnowledgeModelByUuid tenantUuid
  return $ process serverConfig.general.secret encryptedTcKnowledgeModel

modifyTenantConfigKnowledgeModel :: TenantConfigKnowledgeModel -> AppContextM TenantConfigKnowledgeModel
modifyTenantConfigKnowledgeModel tcKnowledgeModel =
  runInTransaction $ do
    serverConfig <- asks serverConfig
    let encryptedUpdatedTcKnowledgeModel = process serverConfig.general.secret tcKnowledgeModel
    updateTenantConfigKnowledgeModel encryptedUpdatedTcKnowledgeModel
    return tcKnowledgeModel

getCurrentTenantConfigProject :: AppContextM TenantConfigProject
getCurrentTenantConfigProject = do
  serverConfig <- asks serverConfig
  encryptedTcProject <- findTenantConfigProject
  return $ process serverConfig.general.secret encryptedTcProject

getTenantConfigProjectByUuid :: U.UUID -> AppContextM TenantConfigProject
getTenantConfigProjectByUuid tenantUuid = do
  serverConfig <- asks serverConfig
  encryptedTcProject <- findTenantConfigProjectByUuid tenantUuid
  return $ process serverConfig.general.secret encryptedTcProject

modifyTenantConfigProject :: TenantConfigProject -> AppContextM TenantConfigProject
modifyTenantConfigProject tcProject =
  runInTransaction $ do
    serverConfig <- asks serverConfig
    let encryptedUpdatedTcProject = process serverConfig.general.secret tcProject
    updateTenantConfigProject encryptedUpdatedTcProject
    return tcProject
