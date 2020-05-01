module Wizard.Api.Resource.Questionnaire.QuestionnaireLabelJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Api.Resource.Questionnaire.QuestionnaireLabelDTO

instance FromJSON LabelDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON LabelDTO where
  toJSON = genericToJSON simpleOptions
