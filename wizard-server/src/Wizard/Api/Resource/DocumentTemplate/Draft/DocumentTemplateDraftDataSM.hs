module Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftDataSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftDataDTO
import Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftDataJM ()
import Wizard.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorSuggestionSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireSuggestionSM ()
import Wizard.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateDrafts

instance ToSchema DocumentTemplateDraftDataDTO where
  declareNamedSchema = toSwagger wizardDocumentTemplateDraftDataDTO
