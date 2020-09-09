module Wizard.Api.Resource.Questionnaire.QuestionnaireEventJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Api.Resource.Questionnaire.QuestionnaireEventDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireReplyJM ()

instance FromJSON QuestionnaireEventDTO where
  parseJSON = genericParseJSON simpleOptions'''

instance ToJSON QuestionnaireEventDTO where
  toJSON = genericToJSON simpleOptions'''

instance FromJSON SetReplyEventDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON SetReplyEventDTO where
  toJSON = genericToJSON simpleOptions

instance FromJSON ClearReplyEventDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON ClearReplyEventDTO where
  toJSON = genericToJSON simpleOptions

instance FromJSON SetLevelEventDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON SetLevelEventDTO where
  toJSON = genericToJSON simpleOptions

instance FromJSON SetLabelsEventDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON SetLabelsEventDTO where
  toJSON = genericToJSON simpleOptions
