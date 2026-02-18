module Wizard.Service.KnowledgeModel.Package.KnowledgeModelPackageService where

import Control.Monad (void)
import Control.Monad.Reader (asks)
import Data.Foldable (traverse_)
import qualified Data.UUID as U

import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.PageMetadata
import Shared.Common.Model.Common.Pageable
import Shared.Common.Model.Common.Sort
import Shared.Coordinate.Model.Coordinate.Coordinate
import Shared.KnowledgeModel.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.KnowledgeModel.Database.DAO.Package.KnowledgeModelPackageEventDAO
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackageEvent
import Wizard.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageChangeDTO
import Wizard.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageDetailDTO
import Wizard.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleDTO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.KnowledgeModel.KnowledgeModelPackageDAO
import Wizard.Database.DAO.Registry.RegistryKnowledgeModelPackageDAO
import Wizard.Database.DAO.Registry.RegistryOrganizationDAO
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Context.AclContext
import Wizard.Model.Context.AppContext
import Wizard.Model.KnowledgeModel.Package.KnowledgeModelPackageDeletionImpact
import Wizard.Model.KnowledgeModel.Package.KnowledgeModelPackageSuggestion
import Wizard.Model.Tenant.Config.TenantConfig
import Wizard.Service.KnowledgeModel.Package.KnowledgeModelPackageMapper
import Wizard.Service.KnowledgeModel.Package.KnowledgeModelPackageUtil
import Wizard.Service.Tenant.Config.ConfigService
import Wizard.Service.Tenant.Limit.LimitService

getPackagesPage :: Maybe String -> Maybe String -> Maybe String -> Maybe Bool -> Pageable -> [Sort] -> AppContextM (Page KnowledgeModelPackageSimpleDTO)
getPackagesPage mOrganizationId mKmId mQuery mOutdated pageable sort = do
  checkPermission _PM_READ_PERM
  tcRegistry <- getCurrentTenantConfigRegistry
  if mOutdated == Just True && not tcRegistry.enabled
    then return $ Page "knowledgeModelPackages" (PageMetadata 0 0 0 0) []
    else do
      packages <- findPackagesPage mOrganizationId mKmId mQuery mOutdated pageable sort
      return . fmap (toSimpleDTO'' tcRegistry.enabled) $ packages

getPackageSuggestions :: Maybe String -> Maybe [Coordinate] -> Maybe [Coordinate] -> Maybe KnowledgeModelPackagePhase -> Maybe Bool -> Pageable -> [Sort] -> AppContextM (Page KnowledgeModelPackageSuggestion)
getPackageSuggestions mQuery mSelectCoordinates mExcludeCoordinates mPhase mNonEditable pageable sort = do
  checkPermission _PM_READ_PERM
  findPackageSuggestionsPage mQuery mSelectCoordinates mExcludeCoordinates mPhase mNonEditable pageable sort

getPackageDetailByUuid :: U.UUID -> Bool -> AppContextM KnowledgeModelPackageDetailDTO
getPackageDetailByUuid pkgUuid excludeDeprecatedVersions = do
  checkViewPermissionToKnowledgeModelPackage (Just pkgUuid) _PM_READ_PERM
  pkg <- findPackageByUuid pkgUuid
  serverConfig <- asks serverConfig
  versions <- getPackageVersions pkg excludeDeprecatedVersions
  pkgRs <- findRegistryPackages
  orgRs <- findRegistryOrganizations
  tcRegistry <- getCurrentTenantConfigRegistry
  return $ toDetailDTO pkg tcRegistry.enabled pkgRs orgRs versions (buildPackageUrl serverConfig.registry.clientUrl pkg pkgRs)

getDependentPackageResources :: U.UUID -> Maybe Bool -> AppContextM [KnowledgeModelPackageDeletionImpact]
getDependentPackageResources uuid mAllVersions = do
  checkPermission _PM_READ_PERM
  case mAllVersions of
    Just True -> do
      pkg <- findPackageByUuid uuid
      allPkgs <- findPackagesByOrganizationIdAndKmId pkg.organizationId pkg.kmId
      findDependentPackageResources (fmap (.uuid) allPkgs)
    _ -> findDependentPackageResources [uuid]

createPackage :: (KnowledgeModelPackage, [KnowledgeModelPackageEvent]) -> AppContextM KnowledgeModelPackageSimpleDTO
createPackage (pkg, pkgEvents) =
  runInTransaction $ do
    checkPackageLimit
    insertPackage pkg
    traverse_ insertPackageEvent pkgEvents
    return . toSimpleDTO $ pkg

modifyPackage :: U.UUID -> KnowledgeModelPackageChangeDTO -> AppContextM KnowledgeModelPackageChangeDTO
modifyPackage pkgUuid reqDto =
  runInTransaction $ do
    checkPermission _PM_WRITE_PERM
    _ <- findPackageByUuid pkgUuid
    updatePackagePhaseAndPublicByUuid pkgUuid reqDto.phase reqDto.public
    return reqDto

deletePackage :: U.UUID -> Maybe Bool -> AppContextM ()
deletePackage uuid mAllVersions =
  runInTransaction $ do
    checkPermission _PM_WRITE_PERM
    case mAllVersions of
      Just True -> do
        pkg <- findPackageByUuid uuid
        void $ deletePackagesFiltered [("organization_id", pkg.organizationId), ("km_id", pkg.kmId)]
      _ -> do
        _ <- findPackageByUuid uuid
        void $ deletePackageByUuid uuid

-- --------------------------------
-- PRIVATE
-- --------------------------------
getPackageVersions :: KnowledgeModelPackage -> Bool -> AppContextM [(U.UUID, String)]
getPackageVersions pkg excludeDeprecatedVersions = do
  allPkgs <- findPackagesByOrganizationIdAndKmId pkg.organizationId pkg.kmId
  return . fmap (\p -> (p.uuid, p.version)) . filter (filterPkg excludeDeprecatedVersions) $ allPkgs
  where
    filterPkg :: Bool -> KnowledgeModelPackage -> Bool
    filterPkg True pkg = pkg.phase == ReleasedKnowledgeModelPackagePhase
    filterPkg False _ = True
