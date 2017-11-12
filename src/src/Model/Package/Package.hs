module Model.Package.Package where

import Control.Lens
import GHC.Generics

import Model.Event.Event

data Package = Package
  { _pkgId :: String
  , _pkgName :: String
  , _pkgGroupId :: String
  , _pkgArtefactId :: String
  , _pkgVersion :: String
  , _pkgDescription :: String
  , _pkgParentPackage :: Maybe Package
  } deriving (Show, Eq, Generic)

data PackageWithEvents = PackageWithEvents
  { _pkgweId :: String
  , _pkgweName :: String
  , _pkgweGroupId :: String
  , _pkgweArtefactId :: String
  , _pkgweVersion :: String
  , _pkgweDescription :: String
  , _pkgweParentPackage :: Maybe PackageWithEvents
  , _pkgweEvents :: [Event]
  } deriving (Show, Eq, Generic)

makeLenses ''Package

makeLenses ''PackageWithEvents
