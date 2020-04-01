module Wizard.Api.Resource.Config.AppConfigChangeDTO where

import GHC.Generics

import Wizard.Model.Config.AppConfig

data AppConfigChangeDTO =
  AppConfigChangeDTO
    { _appConfigChangeDTOOrganization :: AppConfigOrganization
    , _appConfigChangeDTOAuthentication :: AppConfigAuth
    , _appConfigChangeDTOPrivacyAndSupport :: AppConfigPrivacyAndSupport
    , _appConfigChangeDTODashboard :: AppConfigDashboard
    , _appConfigChangeDTOLookAndFeel :: AppConfigLookAndFeel
    , _appConfigChangeDTOKnowledgeModelRegistry :: AppConfigRegistry
    , _appConfigChangeDTOQuestionnaire :: AppConfigQuestionnaire
    }
  deriving (Generic, Show)
