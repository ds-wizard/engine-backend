module Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftDetailSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Branch.BranchSuggestionSM ()
import Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftDetailJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireSuggestionSM ()
import Wizard.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateDrafts
import Wizard.Model.DocumentTemplate.DocumentTemplateDraftDetail
import Wizard.Service.DocumentTemplate.Draft.DocumentTemplateDraftMapper
import WizardLib.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateSM ()
import WizardLib.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import WizardLib.KnowledgeModel.Api.Resource.Package.PackagePatternSM ()

instance ToSchema DocumentTemplateDraftDetail where
  declareNamedSchema = toSwagger (toDraftDetail wizardDocumentTemplateDraft wizardDocumentTemplateDraftData Nothing Nothing)
