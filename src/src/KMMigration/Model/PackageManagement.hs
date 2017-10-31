module KMMigration.Model.PackageManagement where

import KMMigration.Model.Common

type UUID = String

data Package = Package
  { _pkgUuid :: UUID
  , _pkgName :: String
  , _pkgOrganizatio :: String
  , _pkgVersion :: String
  , _pkgParentPackage :: Package
  -- , _pkgEvents :: [Event]
  }
