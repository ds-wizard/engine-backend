module WizardLib.KnowledgeModel.Api.Resource.Package.PackageSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import WizardLib.KnowledgeModel.Api.Resource.Event.EventSM ()
import WizardLib.KnowledgeModel.Api.Resource.Package.PackageDTO
import WizardLib.KnowledgeModel.Api.Resource.Package.PackageJM ()
import WizardLib.KnowledgeModel.Api.Resource.Package.PackagePhaseSM ()
import WizardLib.KnowledgeModel.Database.Migration.Development.Package.Data.Packages

instance ToSchema PackageDTO where
  declareNamedSchema = toSwagger globalPackageDto
