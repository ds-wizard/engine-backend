module WizardLib.KnowledgeModel.Api.Resource.Package.PackagePatternSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import WizardLib.KnowledgeModel.Api.Resource.Package.PackagePatternJM ()
import WizardLib.KnowledgeModel.Database.Migration.Development.Package.Data.Packages
import WizardLib.KnowledgeModel.Model.Package.PackagePattern

instance ToSchema PackagePattern where
  declareNamedSchema = toSwagger packagePatternAll
