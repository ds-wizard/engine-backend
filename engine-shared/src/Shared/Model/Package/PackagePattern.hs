module Shared.Model.Package.PackagePattern where

import GHC.Generics

data PackagePattern = PackagePattern
  { orgId :: Maybe String
  , kmId :: Maybe String
  , minVersion :: Maybe String
  , maxVersion :: Maybe String
  }
  deriving (Show, Eq, Generic)
