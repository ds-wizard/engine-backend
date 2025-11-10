module Wizard.Api.Resource.KnowledgeModel.Package.Publish.KnowledgeModelPackagePublishMigrationSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.KnowledgeModel.Package.Publish.KnowledgeModelPackagePublishMigrationDTO
import Wizard.Api.Resource.KnowledgeModel.Package.Publish.KnowledgeModelPackagePublishMigrationJM ()
import Wizard.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditors

instance ToSchema PackagePublishMigrationDTO where
  declareNamedSchema = toSwagger packagePublishMigrationDTO
