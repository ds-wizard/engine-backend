module Wizard.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorDetailSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventSM ()
import Wizard.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorDetailDTO
import Wizard.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorDetailJM ()
import Wizard.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorStateSM ()
import Wizard.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireReplySM ()
import Wizard.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditors

instance ToSchema KnowledgeModelEditorDetailDTO where
  declareNamedSchema = toSwagger amsterdamKnowledgeModelEditorDetail
