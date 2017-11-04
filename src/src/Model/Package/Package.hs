module Model.Package.Package where

import Control.Lens
import GHC.Generics

data Package = Package
  { _pkgId :: String
  , _pkgName :: String
  , _pkgShortName :: String
  , _pkgVersion :: String
  , _pkgParentPackage :: Maybe Package
  } deriving (Show, Eq, Generic)

makeLenses ''Package
