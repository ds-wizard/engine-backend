module Wizard.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorCreateSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorCreateDTO
import Wizard.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorCreateJM ()
import Wizard.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditors

instance ToSchema KnowledgeModelEditorCreateDTO where
  declareNamedSchema = toSwagger amsterdamKnowledgeModelEditorCreate
