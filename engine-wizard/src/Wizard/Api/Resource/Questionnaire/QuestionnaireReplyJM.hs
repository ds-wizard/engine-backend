module Wizard.Api.Resource.Questionnaire.QuestionnaireReplyJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Api.Resource.Questionnaire.QuestionnaireReplyDTO

instance FromJSON ReplyValueDTO where
  parseJSON = genericParseJSON simpleOptions'''

instance FromJSON IntegrationReplyValueDTO where
  parseJSON = genericParseJSON simpleOptions'''

-- --------------------------------------------------------------------
instance ToJSON ReplyValueDTO where
  toJSON = genericToJSON simpleOptions'''

instance ToJSON IntegrationReplyValueDTO where
  toJSON = genericToJSON simpleOptions'''
