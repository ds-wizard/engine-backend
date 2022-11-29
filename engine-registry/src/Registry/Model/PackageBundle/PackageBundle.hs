module Registry.Model.PackageBundle.PackageBundle where

import GHC.Generics

import Shared.Model.Package.PackageWithEventsRaw

data PackageBundle = PackageBundle
  { bundleId :: String
  , name :: String
  , organizationId :: String
  , kmId :: String
  , version :: String
  , metamodelVersion :: Int
  , packages :: [PackageWithEventsRaw]
  }
  deriving (Show, Eq, Generic)
