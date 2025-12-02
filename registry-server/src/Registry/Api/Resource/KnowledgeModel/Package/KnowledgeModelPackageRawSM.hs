module Registry.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageRawSM where

import Data.Swagger

import Registry.Model.KnowledgeModel.Package.KnowledgeModelPackageRaw

instance ToSchema KnowledgeModelPackageRaw where
  declareNamedSchema _ = return $ NamedSchema (Just "KnowledgeModelPackageRaw") binarySchema
