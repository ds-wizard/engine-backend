module Wizard.Api.Resource.Questionnaire.QuestionnaireLabelJM where

import Data.Aeson

import Shared.Util.JSON (simpleParseJSON, simpleToJSON)
import Wizard.Api.Resource.Questionnaire.QuestionnaireLabelDTO

instance FromJSON LabelDTO where
  parseJSON = simpleParseJSON "_labelDTO"

instance ToJSON LabelDTO where
  toJSON = simpleToJSON "_labelDTO"
