module Wizard.Service.Version.VersionValidation where

import Control.Lens ((^.))

import LensesConfig
import Shared.Service.Coordinate.CoordinateValidation
import Wizard.Service.Package.PackageService
import Wizard.Service.Package.PackageValidation

validateNewPackageVersion pkgVersion branch org = do
  validateVersionFormat False pkgVersion
  mPkg <- getTheNewestPackageByOrganizationIdAndKmId (org ^. organizationId) (branch ^. kmId)
  case mPkg of
    Just pkg -> validateIsVersionHigher pkgVersion (pkg ^. version)
    Nothing -> return ()
