module Wizard.Api.Resource.Questionnaire.QuestionnaireLabelJM where

import Data.Aeson

import Wizard.Api.Resource.Questionnaire.QuestionnaireLabelDTO
import Wizard.Util.JSON (simpleParseJSON, simpleToJSON)

instance FromJSON LabelDTO where
  parseJSON = simpleParseJSON "_labelDTO"

instance ToJSON LabelDTO where
  toJSON = simpleToJSON "_labelDTO"
