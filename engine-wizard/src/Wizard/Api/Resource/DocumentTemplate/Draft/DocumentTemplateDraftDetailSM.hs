module Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftDetailSM where

import Data.Swagger

import Shared.Api.Resource.DocumentTemplate.DocumentTemplateSM ()
import Shared.Api.Resource.Package.PackagePatternSM ()
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import Shared.Util.Swagger
import Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftDetailJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireSuggestionSM ()
import Wizard.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateDrafts
import Wizard.Model.DocumentTemplate.DocumentTemplateDraftDetail
import Wizard.Service.DocumentTemplate.Draft.DocumentTemplateDraftMapper

instance ToSchema DocumentTemplateDraftDetail where
  declareNamedSchema = toSwagger (toDraftDetail wizardDocumentTemplateDraft wizardDocumentTemplateDraftData Nothing)
