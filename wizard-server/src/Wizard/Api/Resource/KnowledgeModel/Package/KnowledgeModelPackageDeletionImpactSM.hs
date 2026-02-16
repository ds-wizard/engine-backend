module Wizard.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageDeletionImpactSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorSuggestionSM ()
import Wizard.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageDeletionImpactJM ()
import Wizard.Api.Resource.Project.ProjectSimpleSM ()
import Wizard.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackageDependents
import Wizard.Model.KnowledgeModel.Package.KnowledgeModelPackageDeletionImpact

instance ToSchema KnowledgeModelPackageDeletionImpact where
  declareNamedSchema = toSwagger netherlandsKmPackageDeletionImpact

instance ToSchema KnowledgeModelPackageReference where
  declareNamedSchema = toSwagger netherlandsKmPackageReference
