module Wizard.Service.Tenant.Config.ConfigMapper where

import Data.Time
import qualified Data.UUID as U

import Shared.OpenId.Model.OpenId.OpenIdClientStyle
import Wizard.Api.Resource.Tenant.Config.TenantConfigChangeDTO
import Wizard.Model.Tenant.Config.TenantConfig
import WizardLib.Public.Model.PersistentCommand.Tenant.Config.CreateAuthenticationConfigCommand
import WizardLib.Public.Model.PersistentCommand.Tenant.Config.UpdateRegistryConfigCommand

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
    , feature = oldConfig.feature
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
    , createdAt = oldConfig.createdAt
    , updatedAt = now
    }

fromLogoDTO :: TenantConfig -> String -> UTCTime -> TenantConfig
fromLogoDTO oldConfig logo now =
  oldConfig {lookAndFeel = oldConfig.lookAndFeel {logoUrl = Just logo}, updatedAt = now}

fromLogoDeleteDTO :: TenantConfig -> UTCTime -> TenantConfig
fromLogoDeleteDTO oldConfig now =
  oldConfig {lookAndFeel = oldConfig.lookAndFeel {logoUrl = Nothing}, updatedAt = now}

fromClientCustomizationDTO :: TenantConfig -> Bool -> UTCTime -> TenantConfig
fromClientCustomizationDTO oldConfig newClientCustomizationEnabled now =
  oldConfig
    { feature = oldConfig.feature {clientCustomizationEnabled = newClientCustomizationEnabled}
    , updatedAt = now
    }

fromAuthenticationCommand :: TenantConfig -> CreateAuthenticationConfigCommand -> UTCTime -> TenantConfig
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
