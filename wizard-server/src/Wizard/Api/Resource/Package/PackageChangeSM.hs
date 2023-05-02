module Wizard.Api.Resource.Package.PackageChangeSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Package.PackageChangeDTO
import Wizard.Api.Resource.Package.PackageChangeJM ()
import Wizard.Service.Package.PackageMapper
import WizardLib.KnowledgeModel.Api.Resource.Package.PackagePhaseSM ()
import WizardLib.KnowledgeModel.Database.Migration.Development.Package.Data.Packages
import WizardLib.KnowledgeModel.Service.Package.PackageMapper

instance ToSchema PackageChangeDTO where
  declareNamedSchema = toSwagger (toChangeDTO (toPackage globalPackage))
