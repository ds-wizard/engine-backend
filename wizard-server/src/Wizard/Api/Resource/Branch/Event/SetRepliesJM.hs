module Wizard.Api.Resource.Branch.Event.SetRepliesJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.Branch.Event.SetRepliesDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireReplyJM ()

instance FromJSON SetRepliesDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SetRepliesDTO where
  toJSON = genericToJSON jsonOptions
