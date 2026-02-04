module Wizard.Service.KnowledgeModel.Package.KnowledgeModelPackageUtil where

import Control.Monad (unless)
import qualified Data.List as L

import Shared.KnowledgeModel.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Wizard.Model.Context.AclContext

selectPackageByOrgIdAndKmId pkg =
  L.find (\p -> p.organizationId == pkg.organizationId && p.kmId == pkg.kmId)

selectOrganizationByOrgId pkg = L.find (\org -> org.organizationId == pkg.organizationId)

checkViewPermissionToKnowledgeModelPackage Nothing orCheckThisPerm = checkPermission orCheckThisPerm
checkViewPermissionToKnowledgeModelPackage (Just pkgUuid) orCheckThisPerm = do
  package <- findPackageByUuid pkgUuid
  unless package.public (checkPermission orCheckThisPerm)
