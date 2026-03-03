module Wizard.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorListSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Shared.Coordinate.Api.Resource.Coordinate.CoordinateSM ()
import Wizard.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorListJM ()
import Wizard.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorStateSM ()
import Wizard.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSuggestionSM ()
import Wizard.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditors
import Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditorList

instance ToSchema KnowledgeModelEditorList where
  declareNamedSchema = toSwagger amsterdamKnowledgeModelEditorList
