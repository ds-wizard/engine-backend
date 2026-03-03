module Wizard.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageDeletionImpactJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorSuggestionJM ()
import Wizard.Api.Resource.Project.ProjectSimpleJM ()
import Wizard.Model.KnowledgeModel.Package.KnowledgeModelPackageDeletionImpact

instance FromJSON KnowledgeModelPackageDeletionImpact where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON KnowledgeModelPackageDeletionImpact where
  toJSON = genericToJSON jsonOptions

instance FromJSON KnowledgeModelPackageReference where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON KnowledgeModelPackageReference where
  toJSON = genericToJSON jsonOptions
