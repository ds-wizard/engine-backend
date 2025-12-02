module Wizard.Api.Resource.KnowledgeModel.Editor.Event.SetRepliesJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.KnowledgeModel.Editor.Event.SetRepliesDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireReplyJM ()

instance FromJSON SetRepliesDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SetRepliesDTO where
  toJSON = genericToJSON jsonOptions
