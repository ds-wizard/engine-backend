module Wizard.Api.Resource.Questionnaire.QuestionnaireLabelListJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Api.Resource.Questionnaire.QuestionnaireLabelListDTO

instance FromJSON QuestionnaireLabelListDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON QuestionnaireLabelListDTO where
  toJSON = genericToJSON simpleOptions
