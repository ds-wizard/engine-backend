module Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftDetailJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.Branch.BranchSuggestionJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireSuggestionJM ()
import Wizard.Model.DocumentTemplate.DocumentTemplateDraftDetail
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateJM ()
import WizardLib.KnowledgeModel.Api.Resource.Package.PackagePatternJM ()

instance FromJSON DocumentTemplateDraftDetail where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplateDraftDetail where
  toJSON = genericToJSON jsonOptions
