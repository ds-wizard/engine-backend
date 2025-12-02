module Wizard.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageDetailSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackagePhaseSM ()
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Wizard.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageDetailDTO
import Wizard.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageDetailJM ()
import Wizard.Api.Resource.Registry.RegistryOrganizationSM ()
import Wizard.Database.Migration.Development.Registry.Data.RegistryOrganizations
import Wizard.Database.Migration.Development.Registry.Data.RegistryPackages
import Wizard.Service.KnowledgeModel.Package.KnowledgeModelPackageMapper

instance ToSchema KnowledgeModelPackageDetailDTO where
  declareNamedSchema =
    toSwagger
      ( toDetailDTO
          globalKmPackage
          False
          [globalRegistryPackage]
          [globalRegistryOrganization]
          ["1.0.0"]
          (Just "https://registry.example.org")
      )
