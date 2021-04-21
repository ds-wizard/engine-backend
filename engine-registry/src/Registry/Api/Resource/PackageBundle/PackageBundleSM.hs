module Registry.Api.Resource.PackageBundle.PackageBundleSM where

import Data.Swagger

import Registry.Api.Resource.Package.PackageRawSM ()
import Registry.Api.Resource.PackageBundle.PackageBundleDTO
import Registry.Api.Resource.PackageBundle.PackageBundleJM ()

instance ToSchema PackageBundleDTO where
  declareNamedSchema _ = return $ NamedSchema (Just "PackageBundleDTO") binarySchema
