module Model.Package.PackageState where

import GHC.Generics

data PackageState
  = UnknownPackageState
  | OutdatedPackageState
  | UpToDatePackageState
  | UnpublishedPackageState
  deriving (Show, Eq, Generic)
