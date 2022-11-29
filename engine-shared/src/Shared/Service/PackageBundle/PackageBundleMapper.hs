module Shared.Service.PackageBundle.PackageBundleMapper where

import Shared.Api.Resource.PackageBundle.PackageBundleDTO
import Shared.Model.PackageBundle.PackageBundle
import qualified Shared.Service.Package.PackageMapper as PM

toDTO :: PackageBundle -> PackageBundleDTO
toDTO pb =
  PackageBundleDTO
    { bundleId = pb.bundleId
    , name = pb.name
    , organizationId = pb.organizationId
    , kmId = pb.kmId
    , version = pb.version
    , metamodelVersion = pb.metamodelVersion
    , packages = PM.toDTO <$> pb.packages
    }
