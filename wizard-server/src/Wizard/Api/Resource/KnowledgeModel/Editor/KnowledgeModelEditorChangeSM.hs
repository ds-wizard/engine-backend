module Wizard.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorChangeSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorChangeDTO
import Wizard.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorChangeJM ()
import Wizard.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditors

instance ToSchema KnowledgeModelEditorChangeDTO where
  declareNamedSchema = toSwagger amsterdamKnowledgeModelEditorChange
