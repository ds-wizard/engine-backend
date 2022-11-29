module Registry.Service.PackageBundle.PackageBundleMapper where

import Registry.Api.Resource.PackageBundle.PackageBundleDTO
import Registry.Model.PackageBundle.PackageBundle
import qualified Registry.Service.Package.PackageMapper as PM

toDTO :: PackageBundle -> PackageBundleDTO
toDTO pb =
  PackageBundleDTO
    { bundleId = pb.bundleId
    , name = pb.name
    , organizationId = pb.organizationId
    , kmId = pb.kmId
    , version = pb.version
    , metamodelVersion = pb.metamodelVersion
    , packages = PM.toRawDTO <$> pb.packages
    }
