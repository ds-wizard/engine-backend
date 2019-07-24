module Service.Version.VersionValidation where

import Control.Lens ((^.))

import LensesConfig
import Service.Package.PackageService
import Service.Package.PackageValidation

heValidateNewPackageVersion pkgVersion branch org callback =
  heValidateVersionFormat pkgVersion $ do
    eitherMaybePackage <- getTheNewestPackageByOrganizationIdAndKmId (org ^. organizationId) (branch ^. kmId)
    case eitherMaybePackage of
      Right (Just pkg) -> heValidateIsVersionHigher pkgVersion (pkg ^. version) callback
      Right Nothing -> callback
      Left error -> return . Left $ error
