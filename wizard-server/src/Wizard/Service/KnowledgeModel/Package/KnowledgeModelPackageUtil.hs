module Wizard.Service.KnowledgeModel.Package.KnowledgeModelPackageUtil where

import Control.Monad (unless)
import qualified Data.List as L

import Shared.Coordinate.Service.Coordinate.CoordinateValidation
import Shared.Coordinate.Util.Coordinate
import Shared.KnowledgeModel.Service.KnowledgeModel.Package.KnowledgeModelPackageUtil
import Wizard.Model.Context.AclContext
import Wizard.Model.Tenant.Config.TenantConfig
import Wizard.Service.Tenant.Config.ConfigMapper
import Wizard.Service.Tenant.Config.ConfigService

selectPackageByOrgIdAndKmId pkg =
  L.find (\p -> p.organizationId == pkg.organizationId && p.kmId == pkg.kmId)

selectOrganizationByOrgId pkg = L.find (\org -> org.organizationId == pkg.organizationId)

checkIfPackageIsPublic Nothing orCheckThisPerm = checkPermission orCheckThisPerm
checkIfPackageIsPublic (Just pkgId) orCheckThisPerm = do
  validateCoordinateFormat False "kmId" pkgId
  tcKnowledgeModel <- getCurrentTenantConfigKnowledgeModel
  let pkgIdSplit = splitCoordinate pkgId
  unless
    ( tcKnowledgeModel.public.enabled
        && fitsIntoKMSpecs pkgIdSplit (fmap toPackagePattern tcKnowledgeModel.public.knowledgeModelPackages)
    )
    (checkPermission orCheckThisPerm)
