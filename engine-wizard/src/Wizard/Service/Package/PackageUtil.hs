module Wizard.Service.Package.PackageUtil where

import Control.Lens ((^.))
import Control.Monad (unless)
import qualified Data.List as L

import LensesConfig
import Shared.Model.Package.Package
import Shared.Service.Coordinate.CoordinateValidation
import Shared.Service.Package.PackageUtil
import Shared.Util.Coordinate
import Wizard.Model.Package.PackageList
import Wizard.Model.Package.PackageState
import Wizard.Model.Registry.RegistryPackage
import Wizard.Service.Acl.AclService
import Wizard.Service.Config.AppConfigService

selectPackageByOrgIdAndKmId pkg =
  L.find (\p -> (p ^. organizationId) == (pkg ^. organizationId) && (p ^. kmId) == (pkg ^. kmId))

selectOrganizationByOrgId pkg = L.find (\org -> (org ^. organizationId) == (pkg ^. organizationId))

computePackageState :: [RegistryPackage] -> Package -> PackageState
computePackageState pkgsFromRegistry pkg =
  case selectPackageByOrgIdAndKmId pkg pkgsFromRegistry of
    Just pkgFromRegistry ->
      case compareVersion (pkgFromRegistry ^. remoteVersion) (pkg ^. version) of
        LT -> UnpublishedPackageState
        EQ -> UpToDatePackageState
        GT -> OutdatedPackageState
    Nothing -> UnknownPackageState

computePackageState' :: PackageList -> PackageState
computePackageState' pkg =
  case pkg ^. remoteVersion of
    Just rVersion ->
      case compareVersion rVersion (pkg ^. version) of
        LT -> UnpublishedPackageState
        EQ -> UpToDatePackageState
        GT -> OutdatedPackageState
    Nothing -> UnknownPackageState

checkIfPackageIsPublic Nothing orCheckThisPerm = checkPermission orCheckThisPerm
checkIfPackageIsPublic (Just pkgId) orCheckThisPerm = do
  validateCoordinateFormat False pkgId
  appConfig <- getAppConfig
  let pkgIdSplit = splitCoordinate pkgId
  unless
    (appConfig ^. knowledgeModel . public . enabled &&
     fitsIntoKMSpecs pkgIdSplit (appConfig ^. knowledgeModel . public . packages))
    (checkPermission orCheckThisPerm)
