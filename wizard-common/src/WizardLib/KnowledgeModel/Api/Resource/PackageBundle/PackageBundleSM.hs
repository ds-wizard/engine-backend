module WizardLib.KnowledgeModel.Api.Resource.PackageBundle.PackageBundleSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import WizardLib.KnowledgeModel.Api.Resource.Package.PackageSM ()
import WizardLib.KnowledgeModel.Api.Resource.PackageBundle.PackageBundleDTO
import WizardLib.KnowledgeModel.Api.Resource.PackageBundle.PackageBundleJM ()
import WizardLib.KnowledgeModel.Database.Migration.Development.PackageBundle.Data.PackageBundles
import WizardLib.KnowledgeModel.Service.Package.Bundle.PackageBundleMapper

instance ToSchema PackageBundleDTO where
  declareNamedSchema = toSwagger (toDTO germanyBundle)
