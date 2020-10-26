module Shared.Service.Package.PackageUtil where

import Control.Lens ((^.))
import qualified Data.List as L

import LensesConfig
import Shared.Model.Package.Package
import Shared.Util.Coordinate
import Shared.Util.List (groupBy)

groupPackages :: [Package] -> [[Package]]
groupPackages = groupBy (\p1 p2 -> (p1 ^. organizationId) == (p2 ^. organizationId) && (p1 ^. kmId) == (p2 ^. kmId))

sortPackagesByVersion :: [Package] -> [Package]
sortPackagesByVersion = L.sortBy (\p1 p2 -> compareVersionNeg (p1 ^. version) (p2 ^. version))

upgradePackageVersion :: String -> String -> String
upgradePackageVersion pkgId = buildCoordinate (getOrgIdFromCoordinate pkgId) (getKmIdFromCoordinate pkgId)
