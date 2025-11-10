module Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackagePhaseSM where

import Data.Swagger

import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Bundle.KnowledgeModelBundlePackageJM ()
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage

instance ToSchema KnowledgeModelPackagePhase

instance ToParamSchema KnowledgeModelPackagePhase
