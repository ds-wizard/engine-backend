module Wizard.Api.Resource.KnowledgeModel.Editor.Event.SetRepliesSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.KnowledgeModel.Editor.Event.SetRepliesDTO
import Wizard.Api.Resource.KnowledgeModel.Editor.Event.SetRepliesJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireReplySM ()
import Wizard.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditorEvents

instance ToSchema SetRepliesDTO where
  declareNamedSchema = toSwagger setRepliesDTO
