module Wizard.Service.Config.AppConfigMapper where

import Control.Lens ((&), (.~), (?~), (^.))
import Data.Time

import LensesConfig
import Wizard.Api.Resource.Config.AppConfigChangeDTO
import Wizard.Model.Config.AppConfig

toChangeDTO :: AppConfig -> AppConfigChangeDTO
toChangeDTO config =
  AppConfigChangeDTO
    { _appConfigChangeDTOOrganization = config ^. organization
    , _appConfigChangeDTOAuthentication = config ^. authentication
    , _appConfigChangeDTOPrivacyAndSupport = config ^. privacyAndSupport
    , _appConfigChangeDTODashboard = config ^. dashboard
    , _appConfigChangeDTOLookAndFeel = config ^. lookAndFeel
    , _appConfigChangeDTORegistry = config ^. registry
    , _appConfigChangeDTOKnowledgeModel = config ^. knowledgeModel
    , _appConfigChangeDTOQuestionnaire = config ^. questionnaire
    , _appConfigChangeDTOTemplate = config ^. template
    , _appConfigChangeDTOSubmission = config ^. submission
    }

fromChangeDTO :: AppConfigChangeDTO -> AppConfig -> UTCTime -> AppConfig
fromChangeDTO dto oldConfig now =
  AppConfig
    { _appConfigUuid = oldConfig ^. uuid
    , _appConfigOrganization = dto ^. organization
    , _appConfigFeature = oldConfig ^. feature
    , _appConfigAuthentication = dto ^. authentication
    , _appConfigPrivacyAndSupport = dto ^. privacyAndSupport
    , _appConfigDashboard = dto ^. dashboard
    , _appConfigLookAndFeel = dto ^. lookAndFeel
    , _appConfigRegistry = dto ^. registry
    , _appConfigKnowledgeModel = dto ^. knowledgeModel
    , _appConfigQuestionnaire = dto ^. questionnaire
    , _appConfigTemplate = dto ^. template
    , _appConfigSubmission = dto ^. submission
    , _appConfigOwl = oldConfig ^. owl
    , _appConfigCreatedAt = oldConfig ^. createdAt
    , _appConfigUpdatedAt = now
    }

fromLogoDTO :: AppConfig -> String -> UTCTime -> AppConfig
fromLogoDTO oldConfig logo now =
  oldConfig {_appConfigLookAndFeel = (oldConfig ^. lookAndFeel) & logoUrl ?~ logo, _appConfigUpdatedAt = now}

fromLogoDeleteDTO :: AppConfig -> UTCTime -> AppConfig
fromLogoDeleteDTO oldConfig now =
  oldConfig {_appConfigLookAndFeel = (oldConfig ^. lookAndFeel) & logoUrl .~ Nothing, _appConfigUpdatedAt = now}

fromClientCustomizationDTO :: AppConfig -> Bool -> UTCTime -> AppConfig
fromClientCustomizationDTO oldConfig newClientCustomizationEnabled now =
  oldConfig
    { _appConfigFeature = (oldConfig ^. feature) & clientCustomizationEnabled .~ newClientCustomizationEnabled
    , _appConfigUpdatedAt = now
    }
