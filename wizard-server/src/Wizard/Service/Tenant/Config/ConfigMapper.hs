module Wizard.Service.Tenant.Config.ConfigMapper where

import Data.Time
import qualified Data.UUID as U

import Shared.OpenId.Model.OpenId.OpenIdClientStyle
import Wizard.Api.Resource.Tenant.Config.TenantConfigChangeDTO
import Wizard.Model.Tenant.Config.TenantConfig
import WizardLib.Public.Model.PersistentCommand.Tenant.Config.CreateOrUpdateAuthenticationConfigCommand
import WizardLib.Public.Model.PersistentCommand.Tenant.Config.UpdateAiAssistantConfigCommand
import WizardLib.Public.Model.PersistentCommand.Tenant.Config.UpdateAnnouncementConfigCommand
import WizardLib.Public.Model.PersistentCommand.Tenant.Config.UpdateDefaultRoleConfigCommand
import WizardLib.Public.Model.PersistentCommand.Tenant.Config.UpdateLookAndFeelConfigCommand
import WizardLib.Public.Model.PersistentCommand.Tenant.Config.UpdateOrganizationConfigCommand
import WizardLib.Public.Model.PersistentCommand.Tenant.Config.UpdateRegistryConfigCommand
import WizardLib.Public.Model.PersistentCommand.Tenant.Config.UpdateSupportConfigCommand
import WizardLib.Public.Model.Tenant.Config.TenantConfig

toChangeDTO :: TenantConfig -> TenantConfigChangeDTO
toChangeDTO config =
  TenantConfigChangeDTO
    { organization = config.organization
    , authentication = config.authentication
    , privacyAndSupport = config.privacyAndSupport
    , dashboardAndLoginScreen = config.dashboardAndLoginScreen
    , lookAndFeel = config.lookAndFeel
    , registry = config.registry
    , knowledgeModel = config.knowledgeModel
    , questionnaire = config.questionnaire
    , submission = config.submission
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
    , submission = dto.submission
    , owl = oldConfig.owl
    , mailConfigUuid = oldConfig.mailConfigUuid
    , aiAssistant = oldConfig.aiAssistant
    , createdAt = oldConfig.createdAt
    , updatedAt = now
    }

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
