module Wizard.Database.Mapping.KnowledgeModel.Package.KnowledgeModelPackageDeletionImpact where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow

import Wizard.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorSuggestionJM ()
import Wizard.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageDeletionImpactJM ()
import Wizard.Api.Resource.Project.ProjectSimpleJM ()
import Wizard.Model.KnowledgeModel.Package.KnowledgeModelPackageDeletionImpact

instance FromRow KnowledgeModelPackageDeletionImpact where
  fromRow = do
    uuid <- field
    name <- field
    version <- field
    packages <- fieldWith fromJSONField
    editors <- fieldWith fromJSONField
    projects <- fieldWith fromJSONField
    return $ KnowledgeModelPackageDeletionImpact {..}

instance FromRow KnowledgeModelPackageReference
