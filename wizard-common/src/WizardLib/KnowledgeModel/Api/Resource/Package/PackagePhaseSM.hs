module WizardLib.KnowledgeModel.Api.Resource.Package.PackagePhaseSM where

import Data.Swagger

import WizardLib.KnowledgeModel.Api.Resource.Package.PackageJM ()
import WizardLib.KnowledgeModel.Model.Package.Package

instance ToSchema PackagePhase

instance ToParamSchema PackagePhase
