module Wizard.Api.Resource.Questionnaire.QuestionnaireReplyJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Api.Resource.User.UserSuggestionJM ()
import Wizard.Model.Questionnaire.QuestionnaireReply

instance ToJSON Reply where
  toJSON = simpleToJSON "_reply"

instance FromJSON Reply where
  parseJSON = simpleParseJSON "_reply"

instance FromJSON ReplyValue where
  parseJSON = genericParseJSON (createSimpleOptions'''' "Reply")

instance ToJSON ReplyValue where
  toJSON = genericToJSON (createSimpleOptions'''' "Reply")

-- --------------------------------------------------------------------
instance FromJSON IntegrationReplyType where
  parseJSON = genericParseJSON (createSimpleOptions'''' "Type")

instance ToJSON IntegrationReplyType where
  toJSON = genericToJSON (createSimpleOptions'''' "Type")
