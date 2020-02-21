module Wizard.Service.Version.VersionValidation where

import Control.Lens ((^.))

import LensesConfig
import Wizard.Service.Package.PackageService
import Wizard.Service.Package.PackageValidation

validateNewPackageVersion pkgVersion branch org = do
  validateVersionFormat pkgVersion
  mPkg <- getTheNewestPackageByOrganizationIdAndKmId (org ^. organizationId) (branch ^. kmId)
  case mPkg of
    Just pkg -> validateIsVersionHigher pkgVersion (pkg ^. version)
    Nothing -> return ()
