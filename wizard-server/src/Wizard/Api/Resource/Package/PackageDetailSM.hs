module Wizard.Api.Resource.Package.PackageDetailSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Package.PackageDetailDTO
import Wizard.Api.Resource.Package.PackageDetailJM ()
import Wizard.Api.Resource.Registry.RegistryOrganizationSM ()
import Wizard.Database.Migration.Development.Registry.Data.RegistryOrganizations
import Wizard.Database.Migration.Development.Registry.Data.RegistryPackages
import Wizard.Service.Package.PackageMapper
import WizardLib.KnowledgeModel.Api.Resource.Package.PackagePhaseSM ()
import WizardLib.KnowledgeModel.Database.Migration.Development.Package.Data.Packages
import WizardLib.KnowledgeModel.Service.Package.PackageMapper

instance ToSchema PackageDetailDTO where
  declareNamedSchema =
    toSwagger
      ( toDetailDTO
          (toPackage globalPackage)
          [globalRegistryPackage]
          [globalRegistryOrganization]
          ["1.0.0"]
          (Just "https://registry.example.org")
      )
