module Wizard.Service.Package.PackageUtil where

import Control.Monad (unless)
import qualified Data.List as L

import Wizard.Model.Context.AclContext
import Wizard.Model.Tenant.Config.TenantConfig
import Wizard.Service.Tenant.Config.ConfigService
import WizardLib.Common.Service.Coordinate.CoordinateValidation
import WizardLib.Common.Util.Coordinate
import WizardLib.KnowledgeModel.Service.Package.PackageUtil

selectPackageByOrgIdAndKmId pkg =
  L.find (\p -> p.organizationId == pkg.organizationId && p.kmId == pkg.kmId)

selectOrganizationByOrgId pkg = L.find (\org -> org.organizationId == pkg.organizationId)

checkIfPackageIsPublic Nothing orCheckThisPerm = checkPermission orCheckThisPerm
checkIfPackageIsPublic (Just pkgId) orCheckThisPerm = do
  validateCoordinateFormat False pkgId
  tenantConfig <- getCurrentTenantConfig
  let pkgIdSplit = splitCoordinate pkgId
  unless
    ( tenantConfig.knowledgeModel.public.enabled
        && fitsIntoKMSpecs pkgIdSplit tenantConfig.knowledgeModel.public.packages
    )
    (checkPermission orCheckThisPerm)
