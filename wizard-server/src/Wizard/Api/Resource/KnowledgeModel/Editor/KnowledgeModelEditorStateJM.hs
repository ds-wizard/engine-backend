module Wizard.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorStateJM where

import Data.Aeson

import Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditorState

instance ToJSON KnowledgeModelEditorState

instance FromJSON KnowledgeModelEditorState
