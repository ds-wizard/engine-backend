module Wizard.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorSuggestionSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorSuggestionJM ()
import Wizard.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditors
import Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditorSuggestion

instance ToSchema KnowledgeModelEditorSuggestion where
  declareNamedSchema = toSwagger amsterdamKnowledgeModelEditorSuggestion
