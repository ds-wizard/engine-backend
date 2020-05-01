module Shared.Api.Resource.PackageBundle.PackageBundleSM where

import Data.Swagger

import Shared.Api.Resource.Package.PackageSM ()
import Shared.Api.Resource.PackageBundle.PackageBundleDTO
import Shared.Api.Resource.PackageBundle.PackageBundleJM ()
import Shared.Database.Migration.Development.PackageBundle.Data.PackageBundles
import Shared.Service.PackageBundle.PackageBundleMapper
import Shared.Util.Swagger

instance ToSchema PackageBundleDTO where
  declareNamedSchema = simpleToSchema (toDTO germanyBundle)
