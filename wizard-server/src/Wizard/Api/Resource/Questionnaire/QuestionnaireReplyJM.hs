module Wizard.Api.Resource.Questionnaire.QuestionnaireReplyJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Model.Questionnaire.QuestionnaireReply
import WizardLib.Public.Api.Resource.User.UserSuggestionJM ()

instance ToJSON Reply where
  toJSON = genericToJSON jsonOptions

instance FromJSON Reply where
  parseJSON = genericParseJSON jsonOptions

instance FromJSON ReplyValue where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "type")

instance ToJSON ReplyValue where
  toJSON = genericToJSON (jsonOptionsWithTypeField "type")

-- --------------------------------------------------------------------
instance FromJSON IntegrationReplyType where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "type")

instance ToJSON IntegrationReplyType where
  toJSON = genericToJSON (jsonOptionsWithTypeField "type")
