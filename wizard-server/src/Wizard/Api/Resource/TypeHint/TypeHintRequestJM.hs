module Wizard.Api.Resource.TypeHint.TypeHintRequestJM where

import Control.Monad
import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.TypeHint.TypeHintRequestDTO
import WizardLib.KnowledgeModel.Api.Resource.Event.EventJM ()

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
      "BranchIntegrationTypeHintRequest" -> parseJSON (Object o) >>= \v -> return (BranchIntegrationTypeHintRequest' v)
      "BranchQuestionTypeHintRequest" -> parseJSON (Object o) >>= \v -> return (BranchQuestionTypeHintRequest' v)
      "QuestionnaireTypeHintRequest" -> parseJSON (Object o) >>= \v -> return (QuestionnaireTypeHintRequest' v)
      _ -> fail "One of the integrations has unsupported requestType"
  parseJSON _ = mzero

instance FromJSON BranchIntegrationTypeHintRequest where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "requestType")

instance ToJSON BranchIntegrationTypeHintRequest where
  toJSON = genericToJSON (jsonOptionsWithTypeField "requestType")

instance FromJSON BranchQuestionTypeHintRequest where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "requestType")

instance ToJSON BranchQuestionTypeHintRequest where
  toJSON = genericToJSON (jsonOptionsWithTypeField "requestType")

instance FromJSON QuestionnaireTypeHintRequest where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "requestType")

instance ToJSON QuestionnaireTypeHintRequest where
  toJSON = genericToJSON (jsonOptionsWithTypeField "requestType")
