module Wizard.Service.Config.App.AppConfigMapper where

import Data.Time

import Wizard.Api.Resource.Config.AppConfigChangeDTO
import Wizard.Model.Config.AppConfig

toChangeDTO :: AppConfig -> AppConfigChangeDTO
toChangeDTO config =
  AppConfigChangeDTO
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

fromChangeDTO :: AppConfigChangeDTO -> AppConfig -> UTCTime -> AppConfig
fromChangeDTO dto oldConfig now =
  AppConfig
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

fromLogoDTO :: AppConfig -> String -> UTCTime -> AppConfig
fromLogoDTO oldConfig logo now =
  oldConfig {lookAndFeel = oldConfig.lookAndFeel {logoUrl = Just logo}, updatedAt = now}

fromLogoDeleteDTO :: AppConfig -> UTCTime -> AppConfig
fromLogoDeleteDTO oldConfig now =
  oldConfig {lookAndFeel = oldConfig.lookAndFeel {logoUrl = Nothing}, updatedAt = now}

fromClientCustomizationDTO :: AppConfig -> Bool -> UTCTime -> AppConfig
fromClientCustomizationDTO oldConfig newClientCustomizationEnabled now =
  oldConfig
    { feature = oldConfig.feature {clientCustomizationEnabled = newClientCustomizationEnabled}
    , updatedAt = now
    }
