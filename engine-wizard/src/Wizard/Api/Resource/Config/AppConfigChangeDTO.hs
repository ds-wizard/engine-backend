module Wizard.Api.Resource.Config.AppConfigChangeDTO where

import GHC.Generics

import Wizard.Model.Config.AppConfig

data AppConfigChangeDTO = AppConfigChangeDTO
  { organization :: AppConfigOrganization
  , authentication :: AppConfigAuth
  , privacyAndSupport :: AppConfigPrivacyAndSupport
  , dashboard :: AppConfigDashboard
  , lookAndFeel :: AppConfigLookAndFeel
  , registry :: AppConfigRegistry
  , knowledgeModel :: AppConfigKnowledgeModel
  , questionnaire :: AppConfigQuestionnaire
  , submission :: AppConfigSubmission
  }
  deriving (Generic, Show)
