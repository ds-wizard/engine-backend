module Wizard.Service.Tenant.Config.ConfigMapper where

import Data.Time
import qualified Data.UUID as U

import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackagePattern
import Shared.OpenId.Model.OpenId.OpenIdClientStyle
import Wizard.Api.Resource.Tenant.Config.TenantConfigChangeDTO
import Wizard.Model.Tenant.Config.TenantConfig
import Wizard.Model.Tenant.Config.TenantConfigSubmissionServiceSimple
import WizardLib.Public.Api.Resource.Tenant.Config.TenantConfigChangeDTO
import WizardLib.Public.Model.PersistentCommand.Tenant.Config.CreateOrUpdateAuthenticationConfigCommand
import WizardLib.Public.Model.PersistentCommand.Tenant.Config.UpdateAnnouncementConfigCommand
import WizardLib.Public.Model.PersistentCommand.Tenant.Config.UpdateDefaultRoleConfigCommand
import WizardLib.Public.Model.PersistentCommand.Tenant.Config.UpdateFeaturesConfigCommand
import WizardLib.Public.Model.PersistentCommand.Tenant.Config.UpdateLookAndFeelConfigCommand
import WizardLib.Public.Model.PersistentCommand.Tenant.Config.UpdateOrganizationConfigCommand
import WizardLib.Public.Model.PersistentCommand.Tenant.Config.UpdateRegistryConfigCommand
import WizardLib.Public.Model.PersistentCommand.Tenant.Config.UpdateSupportConfigCommand
import WizardLib.Public.Model.Tenant.Config.TenantConfig
import WizardLib.Public.Service.Tenant.Config.ConfigMapper

toChangeDTO
  :: TenantConfigOrganizationChangeDTO
  -> TenantConfigAuthenticationChangeDTO
  -> TenantConfigPrivacyAndSupportChangeDTO
  -> TenantConfigDashboardAndLoginScreenChangeDTO
  -> TenantConfigLookAndFeelChangeDTO
  -> TenantConfigRegistryChangeDTO
  -> TenantConfigKnowledgeModelChangeDTO
  -> TenantConfigProjectChangeDTO
  -> TenantConfigSubmissionChangeDTO
  -> TenantConfigFeaturesChangeDTO
  -> TenantConfigChangeDTO
toChangeDTO organization authentication privacyAndSupport dashboardAndLoginScreen lookAndFeel registry knowledgeModel project submission features = TenantConfigChangeDTO {..}

toSubmissionServiceSimple :: TenantConfigSubmissionService -> TenantConfigSubmissionServiceSimple
toSubmissionServiceSimple config =
  TenantConfigSubmissionServiceSimple
    { sId = config.sId
    , name = config.name
    , description = config.description
    }

toTenantConfig
  :: TenantConfigOrganization
  -> TenantConfigAuthentication
  -> TenantConfigPrivacyAndSupport
  -> TenantConfigDashboardAndLoginScreen
  -> TenantConfigLookAndFeel
  -> TenantConfigRegistry
  -> TenantConfigKnowledgeModel
  -> TenantConfigProject
  -> TenantConfigSubmission
  -> TenantConfigFeatures
  -> TenantConfigOwl
  -> TenantConfig
toTenantConfig organization authentication privacyAndSupport dashboardAndLoginScreen lookAndFeel registry knowledgeModel project submission features owl =
  let uuid = organization.tenantUuid
      mailConfigUuid = Nothing
      createdAt = organization.createdAt
      updatedAt = organization.updatedAt
   in TenantConfig {..}

fromOrganizationChangeDTO :: TenantConfigOrganizationChangeDTO -> U.UUID -> UTCTime -> UTCTime -> TenantConfigOrganization
fromOrganizationChangeDTO TenantConfigOrganizationChangeDTO {..} tenantUuid createdAt updatedAt = TenantConfigOrganization {..}

fromAuthenticationChangeDTO :: TenantConfigAuthenticationChangeDTO -> U.UUID -> UTCTime -> UTCTime -> TenantConfigAuthentication
fromAuthenticationChangeDTO a@TenantConfigAuthenticationChangeDTO {..} tenantUuid createdAt updatedAt =
  let services = fmap (\c -> fromAuthenticationExternalServiceChangeDTO c tenantUuid createdAt updatedAt) a.external.services
      external = TenantConfigAuthenticationExternal {..}
   in TenantConfigAuthentication {..}

fromAuthenticationExternalServiceChangeDTO :: TenantConfigAuthenticationExternalServiceChangeDTO -> U.UUID -> UTCTime -> UTCTime -> TenantConfigAuthenticationExternalService
fromAuthenticationExternalServiceChangeDTO TenantConfigAuthenticationExternalServiceChangeDTO {..} tenantUuid createdAt updatedAt = TenantConfigAuthenticationExternalService {..}

fromPrivacyAndSupportChangeDTO :: TenantConfigPrivacyAndSupportChangeDTO -> U.UUID -> UTCTime -> UTCTime -> TenantConfigPrivacyAndSupport
fromPrivacyAndSupportChangeDTO TenantConfigPrivacyAndSupportChangeDTO {..} tenantUuid createdAt updatedAt = TenantConfigPrivacyAndSupport {..}

fromDashboardAndLoginScreenChangeDTO :: TenantConfigDashboardAndLoginScreenChangeDTO -> U.UUID -> UTCTime -> UTCTime -> TenantConfigDashboardAndLoginScreen
fromDashboardAndLoginScreenChangeDTO a@TenantConfigDashboardAndLoginScreenChangeDTO {..} tenantUuid createdAt updatedAt =
  let announcements = zipWith (\i c -> fromDashboardAndLoginScreenAnnouncementChangeDTO c tenantUuid i createdAt updatedAt) [0 ..] a.announcements
   in TenantConfigDashboardAndLoginScreen {..}

fromRegistryChangeDTO :: TenantConfigRegistryChangeDTO -> U.UUID -> UTCTime -> UTCTime -> TenantConfigRegistry
fromRegistryChangeDTO TenantConfigRegistryChangeDTO {..} tenantUuid createdAt updatedAt = TenantConfigRegistry {..}

fromKnowledgeModelChangeDTO :: TenantConfigKnowledgeModelChangeDTO -> U.UUID -> UTCTime -> UTCTime -> TenantConfigKnowledgeModel
fromKnowledgeModelChangeDTO dto@TenantConfigKnowledgeModelChangeDTO {..} tenantUuid createdAt updatedAt =
  let knowledgeModelPackages = zipWith (\i f -> fromKnowledgeModelPublicPackagePatternChangeDTO f tenantUuid i createdAt updatedAt) [0 ..] dto.public.knowledgeModelPackages
      public = TenantConfigKnowledgeModelPublic {enabled = dto.public.enabled, knowledgeModelPackages = knowledgeModelPackages}
   in TenantConfigKnowledgeModel {..}

fromKnowledgeModelPublicPackagePatternChangeDTO :: KnowledgeModelPackagePattern -> U.UUID -> Int -> UTCTime -> UTCTime -> TenantConfigKnowledgeModelPublicPackagePattern
fromKnowledgeModelPublicPackagePatternChangeDTO KnowledgeModelPackagePattern {..} tenantUuid position createdAt updatedAt = TenantConfigKnowledgeModelPublicPackagePattern {..}

fromProjectChangeDTO :: TenantConfigProjectChangeDTO -> U.UUID -> UTCTime -> UTCTime -> TenantConfigProject
fromProjectChangeDTO TenantConfigProjectChangeDTO {..} tenantUuid createdAt updatedAt = TenantConfigProject {..}

fromSubmissionChangeDTO :: TenantConfigSubmissionChangeDTO -> U.UUID -> UTCTime -> UTCTime -> TenantConfigSubmission
fromSubmissionChangeDTO dto@TenantConfigSubmissionChangeDTO {..} tenantUuid createdAt updatedAt =
  let services = fmap (\s -> fromSubmissionServiceChangeDTO s tenantUuid createdAt updatedAt) dto.services
   in TenantConfigSubmission {..}

fromSubmissionServiceChangeDTO :: TenantConfigSubmissionServiceChangeDTO -> U.UUID -> UTCTime -> UTCTime -> TenantConfigSubmissionService
fromSubmissionServiceChangeDTO dto@TenantConfigSubmissionServiceChangeDTO {..} tenantUuid createdAt updatedAt =
  let supportedFormats = fmap (\f -> fromSubmissionServiceSupportedFormatChangeDTO f tenantUuid dto.sId) dto.supportedFormats
      request = fromSubmissionServiceRequestChangeDTO dto.request
   in TenantConfigSubmissionService {..}

fromSubmissionServiceSupportedFormatChangeDTO :: TenantConfigSubmissionServiceSupportedFormatChangeDTO -> U.UUID -> String -> TenantConfigSubmissionServiceSupportedFormat
fromSubmissionServiceSupportedFormatChangeDTO TenantConfigSubmissionServiceSupportedFormatChangeDTO {..} tenantUuid serviceId = TenantConfigSubmissionServiceSupportedFormat {..}

fromSubmissionServiceRequestChangeDTO :: TenantConfigSubmissionServiceRequestChangeDTO -> TenantConfigSubmissionServiceRequest
fromSubmissionServiceRequestChangeDTO dto@TenantConfigSubmissionServiceRequestChangeDTO {..} =
  let multipart = fromSubmissionServiceRequestMultipartChangeDTO dto.multipart
   in TenantConfigSubmissionServiceRequest {..}

fromSubmissionServiceRequestMultipartChangeDTO :: TenantConfigSubmissionServiceRequestMultipartChangeDTO -> TenantConfigSubmissionServiceRequestMultipart
fromSubmissionServiceRequestMultipartChangeDTO TenantConfigSubmissionServiceRequestMultipartChangeDTO {..} =
  TenantConfigSubmissionServiceRequestMultipart {..}

fromFeaturesChangeDTO :: TenantConfigFeaturesChangeDTO -> TenantConfigFeatures -> U.UUID -> UTCTime -> UTCTime -> TenantConfigFeatures
fromFeaturesChangeDTO dto oldConfig tenantUuid createdAt updatedAt =
  let aiAssistantEnabled = oldConfig.aiAssistantEnabled
      toursEnabled = dto.toursEnabled
   in TenantConfigFeatures {..}

fromAuthenticationCommand :: TenantConfigAuthentication -> CreateOrUpdateAuthenticationConfigCommand -> UTCTime -> TenantConfigAuthentication
fromAuthenticationCommand oldConfig command now =
  oldConfig
    { external =
        oldConfig.external
          { services =
              [ TenantConfigAuthenticationExternalService
                  { aId = command.aId
                  , name = command.name
                  , url = command.url
                  , clientId = U.toString command.clientId
                  , clientSecret = command.clientSecret
                  , parameters = []
                  , style =
                      OpenIdClientStyle
                        { icon = Nothing
                        , background = Nothing
                        , color = Nothing
                        }
                  , tenantUuid = command.tenantUuid
                  , createdAt = now
                  , updatedAt = now
                  }
              ]
          }
    , updatedAt = now
    }

fromRegistry :: TenantConfigRegistry -> UpdateRegistryConfigCommand -> UTCTime -> TenantConfigRegistry
fromRegistry oldConfig command now =
  oldConfig
    { enabled = command.enabled
    , token = command.token
    , updatedAt = now
    }

fromLookAndFeel :: TenantConfigLookAndFeel -> UpdateLookAndFeelConfigCommand -> UTCTime -> TenantConfigLookAndFeel
fromLookAndFeel oldConfig command now =
  oldConfig
    { appTitle = command.appTitle
    , appTitleShort = command.appTitleShort
    , logoUrl = command.logoUrl
    , primaryColor = command.primaryColor
    , illustrationsColor = command.illustrationsColor
    , updatedAt = now
    }

fromPrivacyAndSupport :: TenantConfigPrivacyAndSupport -> UpdateSupportConfigCommand -> UTCTime -> TenantConfigPrivacyAndSupport
fromPrivacyAndSupport oldConfig command now =
  oldConfig
    { supportEmail = command.supportEmail
    , supportSiteName = command.supportSiteName
    , supportSiteUrl = command.supportSiteUrl
    , supportSiteIcon = command.supportSiteIcon
    , updatedAt = now
    }

fromDefaultRole :: TenantConfigAuthentication -> UpdateDefaultRoleConfigCommand -> UTCTime -> TenantConfigAuthentication
fromDefaultRole oldConfig command now =
  oldConfig
    { defaultRole = command.defaultRole
    , updatedAt = now
    }

fromAnnouncements :: TenantConfigDashboardAndLoginScreen -> UpdateAnnouncementConfigCommand -> UTCTime -> TenantConfigDashboardAndLoginScreen
fromAnnouncements oldConfig command now =
  oldConfig
    { announcements = command.announcements
    , updatedAt = now
    }

fromFeatures :: TenantConfigFeatures -> UpdateFeaturesConfigCommand -> UTCTime -> TenantConfigFeatures
fromFeatures oldConfig command now =
  oldConfig
    { aiAssistantEnabled = command.aiAssistantEnabled
    , toursEnabled = command.toursEnabled
    , updatedAt = now
    }

fromOrganization :: TenantConfigOrganization -> UpdateOrganizationConfigCommand -> UTCTime -> TenantConfigOrganization
fromOrganization oldConfig command now =
  oldConfig
    { name = command.name
    , description = command.description
    , organizationId = command.organizationId
    , affiliations = command.affiliations
    , updatedAt = now
    }

toPackagePattern :: TenantConfigKnowledgeModelPublicPackagePattern -> KnowledgeModelPackagePattern
toPackagePattern TenantConfigKnowledgeModelPublicPackagePattern {..} = KnowledgeModelPackagePattern {..}
