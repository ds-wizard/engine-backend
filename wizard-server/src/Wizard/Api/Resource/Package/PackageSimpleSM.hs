module Wizard.Api.Resource.Package.PackageSimpleSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Package.PackageSimpleDTO
import Wizard.Api.Resource.Package.PackageSimpleJM ()
import Wizard.Api.Resource.Registry.RegistryOrganizationSM ()
import Wizard.Service.Package.PackageMapper
import WizardLib.KnowledgeModel.Api.Resource.Package.PackagePhaseSM ()
import WizardLib.KnowledgeModel.Database.Migration.Development.Package.Data.Packages
import WizardLib.KnowledgeModel.Model.Package.PackageSimple
import WizardLib.KnowledgeModel.Service.Package.PackageMapper

instance ToSchema PackageSimpleDTO where
  declareNamedSchema = toSwagger (toSimpleDTO (toPackage globalPackage))

instance ToSchema PackageSimple where
  declareNamedSchema = toSwagger (toSimple . toPackage $ globalPackage)
