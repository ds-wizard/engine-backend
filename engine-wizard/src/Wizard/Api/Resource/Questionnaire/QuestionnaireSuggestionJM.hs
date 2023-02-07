module Wizard.Api.Resource.Questionnaire.QuestionnaireSuggestionJM where

import Data.Aeson

import Shared.Util.Aeson
import Wizard.Model.Questionnaire.QuestionnaireSuggestion

instance FromJSON QuestionnaireSuggestion where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON QuestionnaireSuggestion where
  toJSON = genericToJSON jsonOptions
