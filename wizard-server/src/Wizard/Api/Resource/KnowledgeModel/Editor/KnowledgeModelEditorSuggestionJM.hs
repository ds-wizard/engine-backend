module Wizard.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorSuggestionJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditorSuggestion

instance FromJSON KnowledgeModelEditorSuggestion where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON KnowledgeModelEditorSuggestion where
  toJSON = genericToJSON jsonOptions
