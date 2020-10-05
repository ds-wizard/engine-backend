module Shared.Model.Package.PackageGroup where

import GHC.Generics

import Shared.Model.Package.Package

data PackageGroup =
  PackageGroup
    { _packageGroupOrganizationId :: String
    , _packageGroupKmId :: String
    , _packageGroupVersions :: [Package]
    }
  deriving (Show, Eq, Generic)
