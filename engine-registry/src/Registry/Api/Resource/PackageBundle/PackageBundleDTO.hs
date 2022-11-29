module Registry.Api.Resource.PackageBundle.PackageBundleDTO where

import GHC.Generics
import Registry.Api.Resource.Package.PackageRawDTO

data PackageBundleDTO = PackageBundleDTO
  { bundleId :: String
  , name :: String
  , organizationId :: String
  , kmId :: String
  , version :: String
  , metamodelVersion :: Int
  , packages :: [PackageRawDTO]
  }
  deriving (Show, Eq, Generic)
