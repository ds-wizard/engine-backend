module Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftDetailJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateJM ()
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackagePatternJM ()
import Wizard.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorSuggestionJM ()
import Wizard.Api.Resource.Project.ProjectSuggestionJM ()
import Wizard.Model.DocumentTemplate.DocumentTemplateDraftDetail

instance FromJSON DocumentTemplateDraftDetail where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplateDraftDetail where
  toJSON = genericToJSON jsonOptions
