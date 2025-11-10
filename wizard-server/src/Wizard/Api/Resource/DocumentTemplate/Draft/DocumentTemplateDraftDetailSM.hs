module Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftDetailSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Shared.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateSM ()
import Shared.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFormats
import Shared.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackagePatternSM ()
import Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftDetailJM ()
import Wizard.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorSuggestionSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireSuggestionSM ()
import Wizard.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateDrafts
import Wizard.Model.DocumentTemplate.DocumentTemplateDraftDetail
import Wizard.Service.DocumentTemplate.Draft.DocumentTemplateDraftMapper

instance ToSchema DocumentTemplateDraftDetail where
  declareNamedSchema = toSwagger (toDraftDetail wizardDocumentTemplateDraft wizardDocumentTemplateFormats wizardDocumentTemplateDraftData Nothing Nothing)
