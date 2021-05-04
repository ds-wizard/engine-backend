module Registry.Api.Resource.Package.PackageRawSM where

import Data.Swagger

import Registry.Api.Resource.Package.PackageRawDTO
import Shared.Util.Swagger

instance ToSchema PackageRawDTO where
  declareNamedSchema _ = return $ NamedSchema (Just "PackageRawDTO") binarySchema
