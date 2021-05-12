module Shared.Model.Package.PackageGroup where

import GHC.Generics

data PackageGroup =
  PackageGroup
    { _packageGroupOrganizationId :: String
    , _packageGroupKmId :: String
    , _packageGroupVersions :: String
    }
  deriving (Show, Eq, Generic)
