module WizardLib.KnowledgeModel.Database.DAO.Package.PackageDAO where

import Control.Monad.Reader (asks)
import Data.String (fromString)
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import GHC.Int

import Shared.Common.Database.DAO.Common
import Shared.Common.Model.Context.AppContext
import WizardLib.KnowledgeModel.Database.Mapping.Package.Package ()
import WizardLib.KnowledgeModel.Database.Mapping.Package.PackageWithEvents ()
import WizardLib.KnowledgeModel.Database.Mapping.Package.PackageWithEventsRaw ()
import WizardLib.KnowledgeModel.Model.Package.Package
import WizardLib.KnowledgeModel.Model.Package.PackageWithEvents
import WizardLib.KnowledgeModel.Model.Package.PackageWithEventsRaw

entityName = "package"

findPackages :: AppContextC s sc m => m [Package]
findPackages = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid]

findPackageWithEvents :: AppContextC s sc m => m [PackageWithEvents]
findPackageWithEvents = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid]

findPackageWithEventsRawById :: AppContextC s sc m => String -> m PackageWithEventsRaw
findPackageWithEventsRawById id = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn entityName [tenantQueryUuid tenantUuid, ("id", id)]

findPackagesFiltered :: AppContextC s sc m => [(String, String)] -> m [Package]
findPackagesFiltered queryParams = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesByFn entityName (tenantQueryUuid tenantUuid : queryParams)

findPackagesByOrganizationIdAndKmId :: AppContextC s sc m => String -> String -> m [Package]
findPackagesByOrganizationIdAndKmId organizationId kmId = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid, ("organization_id", organizationId), ("km_id", kmId)]

findPackagesByPreviousPackageId :: AppContextC s sc m => String -> m [Package]
findPackagesByPreviousPackageId previousPackageId = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid, ("previous_package_id", previousPackageId)]

findPackagesByForkOfPackageId :: AppContextC s sc m => String -> m [Package]
findPackagesByForkOfPackageId forkOfPackageId = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid, ("fork_of_package_id", forkOfPackageId)]

findSeriesOfPackagesRecursiveById :: (AppContextC s sc m, FromRow packageWithEvents) => String -> m [packageWithEvents]
findSeriesOfPackagesRecursiveById pkgId = do
  tenantUuid <- asks (.tenantUuid')
  let sql =
        fromString
          "WITH RECURSIVE recursive AS ( \
          \  SELECT *, 1 as level \
          \  FROM package \
          \  WHERE tenant_uuid = ? AND id = ? \
          \  UNION ALL \
          \  SELECT pkg.*, level + 1 as level \
          \  FROM package pkg \
          \  INNER JOIN recursive r ON pkg.id = r.previous_package_id AND pkg.tenant_uuid = ? \
          \) \
          \SELECT id, name, organization_id, km_id, version, metamodel_version, description, readme, license, previous_package_id, fork_of_package_id, merge_checkpoint_package_id, events, created_at, tenant_uuid, phase, non_editable \
          \FROM recursive \
          \ORDER BY level DESC;"
  let params = [U.toString tenantUuid, pkgId, U.toString tenantUuid]
  logQuery sql params
  let action conn = query conn sql params
  runDB action

findVersionsForPackage :: AppContextC s sc m => String -> String -> m [String]
findVersionsForPackage orgId kmId = do
  tenantUuid <- asks (.tenantUuid')
  let sql = fromString "SELECT version FROM package WHERE tenant_uuid = ? and organization_id = ? and km_id = ?"
  let params = [U.toString tenantUuid, orgId, kmId]
  logQuery sql params
  let action conn = query conn sql params
  versions <- runDB action
  return . fmap fromOnly $ versions

findPackageById :: AppContextC s sc m => String -> m Package
findPackageById id = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn entityName [tenantQueryUuid tenantUuid, ("id", id)]

findPackageById' :: AppContextC s sc m => String -> m (Maybe Package)
findPackageById' id = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn' entityName [tenantQueryUuid tenantUuid, ("id", id)]

findPackageWithEventsById :: AppContextC s sc m => String -> m PackageWithEvents
findPackageWithEventsById id = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn entityName [tenantQueryUuid tenantUuid, ("id", id)]

countPackages :: AppContextC s sc m => m Int
countPackages = do
  tenantUuid <- asks (.tenantUuid')
  createCountByFn entityName tenantCondition [tenantUuid]

countPackagesGroupedByOrganizationIdAndKmId :: AppContextC s sc m => m Int
countPackagesGroupedByOrganizationIdAndKmId = do
  tenantUuid <- asks (.tenantUuid')
  countPackagesGroupedByOrganizationIdAndKmIdWithTenant tenantUuid

countPackagesGroupedByOrganizationIdAndKmIdWithTenant :: AppContextC s sc m => U.UUID -> m Int
countPackagesGroupedByOrganizationIdAndKmIdWithTenant tenantUuid = do
  let sql =
        "SELECT COUNT(*) \
        \FROM (SELECT 1 \
        \      FROM package \
        \      WHERE tenant_uuid = ? \
        \      GROUP BY organization_id, km_id) nested;"
  let params = [U.toString tenantUuid]
  logQuery sql params
  let action conn = query conn sql params
  result <- runDB action
  case result of
    [count] -> return . fromOnly $ count
    _ -> return 0

insertPackage :: AppContextC s sc m => PackageWithEvents -> m Int64
insertPackage = createInsertFn entityName

deletePackages :: AppContextC s sc m => m Int64
deletePackages = createDeleteEntitiesFn entityName

deletePackagesFiltered :: AppContextC s sc m => [(String, String)] -> m Int64
deletePackagesFiltered queryParams = do
  tenantUuid <- asks (.tenantUuid')
  createDeleteEntitiesByFn entityName (tenantQueryUuid tenantUuid : queryParams)

deletePackageById :: AppContextC s sc m => String -> m Int64
deletePackageById id = do
  tenantUuid <- asks (.tenantUuid')
  createDeleteEntityByFn entityName [tenantQueryUuid tenantUuid, ("id", id)]
