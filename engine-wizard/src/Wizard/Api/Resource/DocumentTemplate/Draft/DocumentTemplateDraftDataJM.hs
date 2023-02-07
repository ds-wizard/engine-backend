module Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftDataJM where

import Data.Aeson

import Shared.Util.Aeson
import Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftDataDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireSuggestionJM ()

instance FromJSON DocumentTemplateDraftDataDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplateDraftDataDTO where
  toJSON = genericToJSON jsonOptions
