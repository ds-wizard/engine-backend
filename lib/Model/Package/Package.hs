module Model.Package.Package where

import Control.Lens
import GHC.Generics

import Model.Event.Event

data Package = Package
  { _pkgId :: String
  , _pkgName :: String
  , _pkgGroupId :: String
  , _pkgArtifactId :: String
  , _pkgVersion :: String
  , _pkgDescription :: String
  , _pkgParentPackageId :: Maybe String
  } deriving (Show, Eq, Generic)

data PackageWithEvents = PackageWithEvents
  { _pkgweId :: String
  , _pkgweName :: String
  , _pkgweGroupId :: String
  , _pkgweArtifactId :: String
  , _pkgweVersion :: String
  , _pkgweDescription :: String
  , _pkgweParentPackageId :: Maybe String
  , _pkgweEvents :: [Event]
  } deriving (Show, Eq, Generic)

makeLenses ''Package

makeLenses ''PackageWithEvents
