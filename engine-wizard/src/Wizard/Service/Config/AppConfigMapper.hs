module Wizard.Service.Config.AppConfigMapper where

import Control.Lens ((^.))
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
    , _appConfigChangeDTOKnowledgeModelRegistry = config ^. knowledgeModelRegistry
    , _appConfigChangeDTOQuestionnaire = config ^. questionnaire
    , _appConfigChangeDTOTemplate = config ^. template
    , _appConfigChangeDTOSubmission = config ^. submission
    }

fromChangeDTO :: AppConfigChangeDTO -> AppConfig -> UTCTime -> AppConfig
fromChangeDTO dto oldConfig now =
  AppConfig
    { _appConfigOrganization = dto ^. organization
    , _appConfigAuthentication = dto ^. authentication
    , _appConfigPrivacyAndSupport = dto ^. privacyAndSupport
    , _appConfigDashboard = dto ^. dashboard
    , _appConfigLookAndFeel = dto ^. lookAndFeel
    , _appConfigKnowledgeModelRegistry = dto ^. knowledgeModelRegistry
    , _appConfigQuestionnaire = dto ^. questionnaire
    , _appConfigTemplate = dto ^. template
    , _appConfigSubmission = dto ^. submission
    , _appConfigCreatedAt = oldConfig ^. createdAt
    , _appConfigUpdatedAt = now
    }
