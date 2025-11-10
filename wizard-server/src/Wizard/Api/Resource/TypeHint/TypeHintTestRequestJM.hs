module Wizard.Api.Resource.TypeHint.TypeHintTestRequestJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventJM ()
import Wizard.Api.Resource.TypeHint.TypeHintTestRequestDTO

instance FromJSON TypeHintTestRequestDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TypeHintTestRequestDTO where
  toJSON = genericToJSON jsonOptions
