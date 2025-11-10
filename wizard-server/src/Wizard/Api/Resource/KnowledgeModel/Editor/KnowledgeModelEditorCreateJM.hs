module Wizard.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorCreateJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorCreateDTO

instance FromJSON KnowledgeModelEditorCreateDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON KnowledgeModelEditorCreateDTO where
  toJSON = genericToJSON jsonOptions
