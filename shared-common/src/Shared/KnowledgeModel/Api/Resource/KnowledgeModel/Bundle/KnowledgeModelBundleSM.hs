module Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Bundle.KnowledgeModelBundleSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Bundle.KnowledgeModelBundleJM ()
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Bundle.KnowledgeModelBundlePackageSM ()
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Bundle.KnowledgeModelBundles
import Shared.KnowledgeModel.Model.KnowledgeModel.Bundle.KnowledgeModelBundle

instance ToSchema KnowledgeModelBundle where
  declareNamedSchema = toSwagger netherlandsV2KmBundle
