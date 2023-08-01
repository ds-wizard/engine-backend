module Wizard.Service.Package.PackageUtil where

import Control.Monad (unless)
import qualified Data.List as L

import Wizard.Model.Config.AppConfig
import Wizard.Model.Context.AclContext
import Wizard.Model.Package.PackageList
import Wizard.Model.Package.PackageState
import Wizard.Model.Registry.RegistryPackage
import Wizard.Service.Config.App.AppConfigService
import WizardLib.Common.Service.Coordinate.CoordinateValidation
import WizardLib.Common.Util.Coordinate
import WizardLib.KnowledgeModel.Model.Package.Package
import WizardLib.KnowledgeModel.Service.Package.PackageUtil

selectPackageByOrgIdAndKmId pkg =
  L.find (\p -> p.organizationId == pkg.organizationId && p.kmId == pkg.kmId)

selectOrganizationByOrgId pkg = L.find (\org -> org.organizationId == pkg.organizationId)

computePackageState :: [RegistryPackage] -> Package -> PackageState
computePackageState pkgsFromRegistry pkg =
  case selectPackageByOrgIdAndKmId pkg pkgsFromRegistry of
    Just pkgFromRegistry ->
      case compareVersion pkgFromRegistry.remoteVersion pkg.version of
        LT -> UnpublishedPackageState
        EQ -> UpToDatePackageState
        GT -> OutdatedPackageState
    Nothing -> UnknownPackageState

computePackageState' :: Bool -> PackageList -> PackageState
computePackageState' registryEnabled pkg =
  if registryEnabled
    then pkg.state
    else UnknownPackageState

checkIfPackageIsPublic Nothing orCheckThisPerm = checkPermission orCheckThisPerm
checkIfPackageIsPublic (Just pkgId) orCheckThisPerm = do
  validateCoordinateFormat False pkgId
  appConfig <- getAppConfig
  let pkgIdSplit = splitCoordinate pkgId
  unless
    ( appConfig.knowledgeModel.public.enabled
        && fitsIntoKMSpecs pkgIdSplit appConfig.knowledgeModel.public.packages
    )
    (checkPermission orCheckThisPerm)
