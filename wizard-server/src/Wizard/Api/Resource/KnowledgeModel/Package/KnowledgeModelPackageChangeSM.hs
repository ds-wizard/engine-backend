module Wizard.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageChangeSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackagePhaseSM ()
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Wizard.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageChangeDTO
import Wizard.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageChangeJM ()
import Wizard.Service.KnowledgeModel.Package.KnowledgeModelPackageMapper

instance ToSchema KnowledgeModelPackageChangeDTO where
  declareNamedSchema = toSwagger (toChangeDTO globalKmPackage)
