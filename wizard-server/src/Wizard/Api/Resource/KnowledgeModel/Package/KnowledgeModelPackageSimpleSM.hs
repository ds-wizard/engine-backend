module Wizard.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackagePhaseSM ()
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackageSimple
import Shared.KnowledgeModel.Service.KnowledgeModel.Package.KnowledgeModelPackageMapper
import Wizard.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleDTO
import Wizard.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleJM ()
import Wizard.Api.Resource.Registry.RegistryOrganizationSM ()
import Wizard.Service.KnowledgeModel.Package.KnowledgeModelPackageMapper

instance ToSchema KnowledgeModelPackageSimpleDTO where
  declareNamedSchema = toSwagger (toSimpleDTO globalKmPackage)

instance ToSchema KnowledgeModelPackageSimple where
  declareNamedSchema = toSwagger (toSimple globalKmPackage)
