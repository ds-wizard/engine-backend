module Wizard.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorChangeJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorChangeDTO

instance FromJSON KnowledgeModelEditorChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON KnowledgeModelEditorChangeDTO where
  toJSON = genericToJSON jsonOptions
