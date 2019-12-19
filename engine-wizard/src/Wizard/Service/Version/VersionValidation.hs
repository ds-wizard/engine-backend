module Wizard.Service.Version.VersionValidation where

import Control.Lens ((^.))

import Wizard.LensesConfig
import Wizard.Service.Package.PackageService
import Wizard.Service.Package.PackageValidation

heValidateNewPackageVersion pkgVersion branch org callback =
  heValidateVersionFormat pkgVersion $ do
    eitherMaybePackage <- getTheNewestPackageByOrganizationIdAndKmId (org ^. organizationId) (branch ^. kmId)
    case eitherMaybePackage of
      Right (Just pkg) -> heValidateIsVersionHigher pkgVersion (pkg ^. version) callback
      Right Nothing -> callback
      Left error -> return . Left $ error
