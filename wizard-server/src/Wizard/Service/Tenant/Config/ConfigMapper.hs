module Wizard.Service.Tenant.Config.ConfigMapper where

import Data.Time
import qualified Data.UUID as U

import Shared.OpenId.Model.OpenId.OpenIdClientStyle
import Wizard.Api.Resource.Tenant.Config.TenantConfigChangeDTO
import Wizard.Api.Resource.Tenant.Config.TenantConfigDTO
import Wizard.Model.Tenant.Config.TenantConfig
import Wizard.Model.Tenant.Config.TenantConfigSubmission
import Wizard.Model.Tenant.Config.TenantConfigSubmissionServiceSimple
import WizardLib.Public.Model.PersistentCommand.Tenant.Config.CreateOrUpdateAuthenticationConfigCommand
import WizardLib.Public.Model.PersistentCommand.Tenant.Config.UpdateAiAssistantConfigCommand
import WizardLib.Public.Model.PersistentCommand.Tenant.Config.UpdateAnnouncementConfigCommand
import WizardLib.Public.Model.PersistentCommand.Tenant.Config.UpdateDefaultRoleConfigCommand
import WizardLib.Public.Model.PersistentCommand.Tenant.Config.UpdateLookAndFeelConfigCommand
import WizardLib.Public.Model.PersistentCommand.Tenant.Config.UpdateOrganizationConfigCommand
import WizardLib.Public.Model.PersistentCommand.Tenant.Config.UpdateRegistryConfigCommand
import WizardLib.Public.Model.PersistentCommand.Tenant.Config.UpdateSupportConfigCommand
import WizardLib.Public.Model.Tenant.Config.TenantConfig

toDTO :: TenantConfig -> TenantConfigSubmission -> TenantConfigDTO
toDTO TenantConfig {..} submission = TenantConfigDTO {..}

toChangeDTO :: TenantConfig -> TenantConfigSubmissionChangeDTO -> TenantConfigChangeDTO
toChangeDTO TenantConfig {..} submission = TenantConfigChangeDTO {..}

toSubmissionServiceSimple :: TenantConfigSubmissionService -> TenantConfigSubmissionServiceSimple
toSubmissionServiceSimple config =
  TenantConfigSubmissionServiceSimple
    { sId = config.sId
    , name = config.name
    , description = config.description
    }

fromChangeDTO :: TenantConfigChangeDTO -> TenantConfig -> UTCTime -> TenantConfig
fromChangeDTO dto oldConfig now =
  TenantConfig
    { uuid = oldConfig.uuid
    , organization = dto.organization
    , authentication = dto.authentication
    , privacyAndSupport = dto.privacyAndSupport
    , dashboardAndLoginScreen = dto.dashboardAndLoginScreen
    , lookAndFeel = dto.lookAndFeel
    , registry = dto.registry
    , knowledgeModel = dto.knowledgeModel
    , questionnaire = dto.questionnaire
    , owl = oldConfig.owl
    , mailConfigUuid = oldConfig.mailConfigUuid
    , aiAssistant = oldConfig.aiAssistant
    , createdAt = oldConfig.createdAt
    , updatedAt = now
    }

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

fromAuthenticationCommand :: TenantConfig -> CreateOrUpdateAuthenticationConfigCommand -> UTCTime -> TenantConfig
fromAuthenticationCommand oldConfig command now =
  oldConfig
    { authentication =
        oldConfig.authentication
          { external =
              oldConfig.authentication.external
                { services =
                    [ TenantConfigAuthExternalService
                        { aId = command.aId
                        , name = command.name
                        , url = command.url
                        , clientId = U.toString command.clientId
                        , clientSecret = command.clientSecret
                        , parameteres = []
                        , style =
                            Just
                              OpenIdClientStyle
                                { icon = Nothing
                                , background = Nothing
                                , color = Nothing
                                }
                        }
                    ]
                }
          }
    , updatedAt = now
    }

fromRegistry :: TenantConfig -> UpdateRegistryConfigCommand -> UTCTime -> TenantConfig
fromRegistry oldConfig command now =
  oldConfig
    { registry =
        TenantConfigRegistry
          { enabled = command.enabled
          , token = command.token
          }
    , updatedAt = now
    }

fromLookAndFeel :: TenantConfig -> UpdateLookAndFeelConfigCommand -> UTCTime -> TenantConfig
fromLookAndFeel oldConfig command now =
  oldConfig
    { lookAndFeel =
        oldConfig.lookAndFeel
          { appTitle = command.appTitle
          , appTitleShort = command.appTitleShort
          , logoUrl = command.logoUrl
          , primaryColor = command.primaryColor
          , illustrationsColor = command.illustrationsColor
          }
    , updatedAt = now
    }

fromPrivacyAndSupport :: TenantConfig -> UpdateSupportConfigCommand -> UTCTime -> TenantConfig
fromPrivacyAndSupport oldConfig command now =
  oldConfig
    { privacyAndSupport =
        oldConfig.privacyAndSupport
          { supportEmail = command.supportEmail
          , supportSiteName = command.supportSiteName
          , supportSiteUrl = command.supportSiteUrl
          , supportSiteIcon = command.supportSiteIcon
          }
    , updatedAt = now
    }

fromDefaultRole :: TenantConfig -> UpdateDefaultRoleConfigCommand -> UTCTime -> TenantConfig
fromDefaultRole oldConfig command now =
  oldConfig
    { authentication =
        oldConfig.authentication
          { defaultRole = command.defaultRole
          }
    , updatedAt = now
    }

fromAnnouncements :: TenantConfig -> UpdateAnnouncementConfigCommand -> UTCTime -> TenantConfig
fromAnnouncements oldConfig command now =
  oldConfig
    { dashboardAndLoginScreen =
        oldConfig.dashboardAndLoginScreen
          { announcements = command.announcements
          }
    , updatedAt = now
    }

fromAiAssitant :: TenantConfig -> UpdateAiAssistantConfigCommand -> UTCTime -> TenantConfig
fromAiAssitant oldConfig command now =
  oldConfig
    { aiAssistant =
        oldConfig.aiAssistant
          { enabled = command.enabled
          }
    , updatedAt = now
    }

fromOrganization :: TenantConfig -> UpdateOrganizationConfigCommand -> UTCTime -> TenantConfig
fromOrganization oldConfig command now =
  oldConfig
    { organization =
        TenantConfigOrganization
          { name = command.name
          , description = command.description
          , organizationId = command.organizationId
          , affiliations = command.affiliations
          }
    , updatedAt = now
    }
