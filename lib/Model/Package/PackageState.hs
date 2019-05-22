module Model.Package.PackageState where

data PackageState
  = UnknownPackageState
  | OutdatedPackageState
  | UpToDatePackageState
  | UnpublishedPackageState
  deriving (Show, Eq)
