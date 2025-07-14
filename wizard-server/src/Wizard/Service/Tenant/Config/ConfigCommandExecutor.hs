module Wizard.Service.Tenant.Config.ConfigCommandExecutor where

import Control.Monad.Except (throwError)
import Control.Monad.Reader (liftIO)
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Time
import qualified Data.UUID as U

import Shared.Common.Model.Error.Error
import Shared.Common.Util.Logger
import Shared.PersistentCommand.Model.PersistentCommand.PersistentCommand
import Wizard.Database.DAO.Tenant.Config.TenantConfigDashboardAndLoginScreenDAO
import Wizard.Database.DAO.Tenant.Config.TenantConfigOrganizationDAO
import Wizard.Database.DAO.Tenant.Config.TenantConfigPrivacyAndSupportDAO
import Wizard.Model.Context.AppContext
import Wizard.Service.Tenant.Config.ConfigMapper
import Wizard.Service.Tenant.Config.ConfigService
import WizardLib.Public.Database.DAO.Tenant.Config.TenantConfigAiAssistantDAO
import WizardLib.Public.Database.DAO.Tenant.Config.TenantConfigLookAndFeelDAO
import WizardLib.Public.Model.PersistentCommand.Tenant.Config.CreateOrUpdateAuthenticationConfigCommand
import WizardLib.Public.Model.PersistentCommand.Tenant.Config.UpdateAiAssistantConfigCommand
import WizardLib.Public.Model.PersistentCommand.Tenant.Config.UpdateAnnouncementConfigCommand
import WizardLib.Public.Model.PersistentCommand.Tenant.Config.UpdateDefaultRoleConfigCommand
import WizardLib.Public.Model.PersistentCommand.Tenant.Config.UpdateLookAndFeelConfigCommand
import WizardLib.Public.Model.PersistentCommand.Tenant.Config.UpdateOrganizationConfigCommand
import WizardLib.Public.Model.PersistentCommand.Tenant.Config.UpdateRegistryConfigCommand
import WizardLib.Public.Model.PersistentCommand.Tenant.Config.UpdateSupportConfigCommand

cComponent = "tenant_config"

execute :: PersistentCommand U.UUID -> AppContextM (PersistentCommandState, Maybe String)
execute command
  | command.function == cCreateOrUpdateAuthenticationName = cCreateOrUpdateAuthentication command
  | command.function == cUpdateRegistryName = cUpdateRegistry command
  | command.function == cUpdateLookAndFeelName = cUpdateLookAndFeel command
  | command.function == cUpdateSupportName = cUpdateSupport command
  | command.function == cUpdateDefaultRoleName = cUpdateDefaultRole command
  | command.function == cUpdateAnnouncementsName = cUpdateAnnouncements command
  | command.function == cUpdateAiAssistantName = cUpdateAiAssistant command
  | command.function == cUpdateOrganizationName = cUpdateOrganization command
  | otherwise = throwError . GeneralServerError $ "Unknown command function: " <> command.function

cCreateOrUpdateAuthenticationName = "createOrUpdateAuthentication"

cCreateOrUpdateAuthentication :: PersistentCommand U.UUID -> AppContextM (PersistentCommandState, Maybe String)
cCreateOrUpdateAuthentication persistentCommand = do
  let eCommand = eitherDecode (BSL.pack persistentCommand.body) :: Either String CreateOrUpdateAuthenticationConfigCommand
  case eCommand of
    Right command -> do
      tcAuthentication <- getTenantConfigAuthenticationByUuid persistentCommand.tenantUuid
      now <- liftIO getCurrentTime
      let tcAuthenticationUpdated = fromAuthenticationCommand tcAuthentication command now
      modifyTenantConfigAuthentication tcAuthenticationUpdated
      return (DonePersistentCommandState, Nothing)
    Left error -> return (ErrorPersistentCommandState, Just $ f' "Problem in deserialization of JSON: %s" [error])

cUpdateRegistryName = "updateRegistry"

cUpdateRegistry :: PersistentCommand U.UUID -> AppContextM (PersistentCommandState, Maybe String)
cUpdateRegistry persistentCommand = do
  let eCommand = eitherDecode (BSL.pack persistentCommand.body) :: Either String UpdateRegistryConfigCommand
  case eCommand of
    Right command -> do
      tcRegistry <- getTenantConfigRegistryByUuid command.tenantUuid
      now <- liftIO getCurrentTime
      let tcRegistryUpdated = fromRegistry tcRegistry command now
      modifyTenantConfigRegistry tcRegistryUpdated
      return (DonePersistentCommandState, Nothing)
    Left error -> return (ErrorPersistentCommandState, Just $ f' "Problem in deserialization of JSON: %s" [error])

cUpdateLookAndFeelName = "updateLookAndFeel"

cUpdateLookAndFeel :: PersistentCommand U.UUID -> AppContextM (PersistentCommandState, Maybe String)
cUpdateLookAndFeel persistentCommand = do
  let eCommand = eitherDecode (BSL.pack persistentCommand.body) :: Either String UpdateLookAndFeelConfigCommand
  case eCommand of
    Right command -> do
      tcLookAndFeel <- findTenantConfigLookAndFeelByUuid persistentCommand.tenantUuid
      now <- liftIO getCurrentTime
      let tcLookAndFeelUpdated = fromLookAndFeel tcLookAndFeel command now
      updateTenantConfigLookAndFeel tcLookAndFeelUpdated
      return (DonePersistentCommandState, Nothing)
    Left error -> return (ErrorPersistentCommandState, Just $ f' "Problem in deserialization of JSON: %s" [error])

cUpdateSupportName = "updateSupport"

cUpdateSupport :: PersistentCommand U.UUID -> AppContextM (PersistentCommandState, Maybe String)
cUpdateSupport persistentCommand = do
  let eCommand = eitherDecode (BSL.pack persistentCommand.body) :: Either String UpdateSupportConfigCommand
  case eCommand of
    Right command -> do
      tcPrivacyAndSupport <- findTenantConfigPrivacyAndSupportByUuid persistentCommand.tenantUuid
      now <- liftIO getCurrentTime
      let tcPrivacyAndSupportUpdated = fromPrivacyAndSupport tcPrivacyAndSupport command now
      updateTenantConfigPrivacyAndSupport tcPrivacyAndSupportUpdated
      return (DonePersistentCommandState, Nothing)
    Left error -> return (ErrorPersistentCommandState, Just $ f' "Problem in deserialization of JSON: %s" [error])

cUpdateDefaultRoleName = "updateDefaultRole"

cUpdateDefaultRole :: PersistentCommand U.UUID -> AppContextM (PersistentCommandState, Maybe String)
cUpdateDefaultRole persistentCommand = do
  let eCommand = eitherDecode (BSL.pack persistentCommand.body) :: Either String UpdateDefaultRoleConfigCommand
  case eCommand of
    Right command -> do
      tcAuthentication <- getTenantConfigAuthenticationByUuid persistentCommand.tenantUuid
      now <- liftIO getCurrentTime
      let tcAuthenticationUpdated = fromDefaultRole tcAuthentication command now
      modifyTenantConfigAuthentication tcAuthenticationUpdated
      return (DonePersistentCommandState, Nothing)
    Left error -> return (ErrorPersistentCommandState, Just $ f' "Problem in deserialization of JSON: %s" [error])

cUpdateAnnouncementsName = "updateAnnouncements"

cUpdateAnnouncements :: PersistentCommand U.UUID -> AppContextM (PersistentCommandState, Maybe String)
cUpdateAnnouncements persistentCommand = do
  let eCommand = eitherDecode (BSL.pack persistentCommand.body) :: Either String UpdateAnnouncementConfigCommand
  case eCommand of
    Right command -> do
      tenantConfig <- findTenantConfigDashboardAndLoginScreenByUuid persistentCommand.tenantUuid
      now <- liftIO getCurrentTime
      let updatedTenantConfig = fromAnnouncements tenantConfig command now
      updateTenantConfigDashboardAndLoginScreen updatedTenantConfig
      return (DonePersistentCommandState, Nothing)
    Left error -> return (ErrorPersistentCommandState, Just $ f' "Problem in deserialization of JSON: %s" [error])

cUpdateAiAssistantName = "updateAiAssistant"

cUpdateAiAssistant :: PersistentCommand U.UUID -> AppContextM (PersistentCommandState, Maybe String)
cUpdateAiAssistant persistentCommand = do
  let eCommand = eitherDecode (BSL.pack persistentCommand.body) :: Either String UpdateAiAssistantConfigCommand
  case eCommand of
    Right command -> do
      tcAiAssistant <- findTenantConfigAiAssistantByUuid persistentCommand.tenantUuid
      now <- liftIO getCurrentTime
      let tcAiAssistantUpdated = fromAiAssitant tcAiAssistant command now
      updateTenantConfigAiAssistant tcAiAssistantUpdated
      return (DonePersistentCommandState, Nothing)
    Left error -> return (ErrorPersistentCommandState, Just $ f' "Problem in deserialization of JSON: %s" [error])

cUpdateOrganizationName = "updateOrganization"

cUpdateOrganization :: PersistentCommand U.UUID -> AppContextM (PersistentCommandState, Maybe String)
cUpdateOrganization persistentCommand = do
  let eCommand = eitherDecode (BSL.pack persistentCommand.body) :: Either String UpdateOrganizationConfigCommand
  case eCommand of
    Right command -> do
      tcOrganization <- findTenantConfigOrganizationByUuid persistentCommand.tenantUuid
      now <- liftIO getCurrentTime
      let tcOrganizationUpdated = fromOrganization tcOrganization command now
      updateTenantConfigOrganization tcOrganizationUpdated
      return (DonePersistentCommandState, Nothing)
    Left error -> return (ErrorPersistentCommandState, Just $ f' "Problem in deserialization of JSON: %s" [error])
