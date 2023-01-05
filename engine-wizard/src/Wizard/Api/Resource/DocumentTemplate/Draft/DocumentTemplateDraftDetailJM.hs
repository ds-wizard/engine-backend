module Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftDetailJM where

import Data.Aeson

import Shared.Api.Resource.Package.PackagePatternJM ()
import Shared.Model.DocumentTemplate.DocumentTemplateJM ()
import Shared.Util.Aeson
import Wizard.Api.Resource.Questionnaire.QuestionnaireSuggestionJM ()
import Wizard.Model.DocumentTemplate.DocumentTemplateDraftDetail

instance FromJSON DocumentTemplateDraftDetail where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplateDraftDetail where
  toJSON = genericToJSON jsonOptions
