module Registry.Api.Resource.KnowledgeModel.Bundle.KnowledgeModelBundleSM where

import Data.Swagger

import Registry.Api.Resource.KnowledgeModel.Bundle.KnowledgeModelBundleJM ()
import Registry.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageRawSM ()
import Registry.Model.KnowledgeModel.Bundle.KnowledgeModelBundle

instance ToSchema KnowledgeModelBundle where
  declareNamedSchema _ = return $ NamedSchema (Just "KnowledgeModelBundle") binarySchema
