module Wizard.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorListJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorStateJM ()
import Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditorList

instance FromJSON KnowledgeModelEditorList where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON KnowledgeModelEditorList where
  toJSON = genericToJSON jsonOptions
