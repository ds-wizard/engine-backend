module Wizard.Api.Resource.TypeHint.TypeHintRequestJM where

import Control.Monad
import Data.Aeson

import Shared.Common.Util.Aeson
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventJM ()
import Wizard.Api.Resource.TypeHint.TypeHintRequestDTO

instance FromJSON TypeHintLegacyRequestDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TypeHintLegacyRequestDTO where
  toJSON = genericToJSON jsonOptions

instance ToJSON TypeHintRequestDTO where
  toJSON = toSumJSON

instance FromJSON TypeHintRequestDTO where
  parseJSON (Object o) = do
    requestType <- o .: "requestType"
    case requestType of
      "KnowledgeModelEditorIntegrationTypeHintRequest" -> parseJSON (Object o) >>= \v -> return (KnowledgeModelEditorIntegrationTypeHintRequest' v)
      "KnowledgeModelEditorQuestionTypeHintRequest" -> parseJSON (Object o) >>= \v -> return (KnowledgeModelEditorQuestionTypeHintRequest' v)
      "QuestionnaireTypeHintRequest" -> parseJSON (Object o) >>= \v -> return (QuestionnaireTypeHintRequest' v)
      _ -> fail "One of the integrations has unsupported requestType"
  parseJSON _ = mzero

instance FromJSON KnowledgeModelEditorIntegrationTypeHintRequest where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "requestType")

instance ToJSON KnowledgeModelEditorIntegrationTypeHintRequest where
  toJSON = genericToJSON (jsonOptionsWithTypeField "requestType")

instance FromJSON KnowledgeModelEditorQuestionTypeHintRequest where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "requestType")

instance ToJSON KnowledgeModelEditorQuestionTypeHintRequest where
  toJSON = genericToJSON (jsonOptionsWithTypeField "requestType")

instance FromJSON QuestionnaireTypeHintRequest where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "requestType")

instance ToJSON QuestionnaireTypeHintRequest where
  toJSON = genericToJSON (jsonOptionsWithTypeField "requestType")
