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
  appUuid <- asks (.appUuid')
  createFindEntitiesByFn entityName [appQueryUuid appUuid]

findPackageWithEvents :: AppContextC s sc m => m [PackageWithEvents]
findPackageWithEvents = do
  appUuid <- asks (.appUuid')
  createFindEntitiesByFn entityName [appQueryUuid appUuid]

findPackageWithEventsRawById :: AppContextC s sc m => String -> m PackageWithEventsRaw
findPackageWithEventsRawById id = do
  appUuid <- asks (.appUuid')
  createFindEntityByFn entityName [appQueryUuid appUuid, ("id", id)]

findPackagesFiltered :: AppContextC s sc m => [(String, String)] -> m [Package]
findPackagesFiltered queryParams = do
  appUuid <- asks (.appUuid')
  createFindEntitiesByFn entityName (appQueryUuid appUuid : queryParams)

findPackagesByOrganizationIdAndKmId :: AppContextC s sc m => String -> String -> m [Package]
findPackagesByOrganizationIdAndKmId organizationId kmId = do
  appUuid <- asks (.appUuid')
  createFindEntitiesByFn entityName [appQueryUuid appUuid, ("organization_id", organizationId), ("km_id", kmId)]

findPackagesByPreviousPackageId :: AppContextC s sc m => String -> m [Package]
findPackagesByPreviousPackageId previousPackageId = do
  appUuid <- asks (.appUuid')
  createFindEntitiesByFn entityName [appQueryUuid appUuid, ("previous_package_id", previousPackageId)]

findPackagesByForkOfPackageId :: AppContextC s sc m => String -> m [Package]
findPackagesByForkOfPackageId forkOfPackageId = do
  appUuid <- asks (.appUuid')
  createFindEntitiesByFn entityName [appQueryUuid appUuid, ("fork_of_package_id", forkOfPackageId)]

findSeriesOfPackagesRecursiveById :: (AppContextC s sc m, FromRow packageWithEvents) => String -> m [packageWithEvents]
findSeriesOfPackagesRecursiveById pkgId = do
  appUuid <- asks (.appUuid')
  let sql =
        fromString
          "WITH RECURSIVE recursive AS ( \
          \  SELECT *, 1 as level \
          \  FROM package \
          \  WHERE app_uuid = ? AND id = ? \
          \  UNION ALL \
          \  SELECT pkg.*, level + 1 as level \
          \  FROM package pkg \
          \  INNER JOIN recursive r ON pkg.id = r.previous_package_id AND pkg.app_uuid = ? \
          \) \
          \SELECT id, name, organization_id, km_id, version, metamodel_version, description, readme, license, previous_package_id, fork_of_package_id, merge_checkpoint_package_id, events, created_at, app_uuid, phase, non_editable \
          \FROM recursive \
          \ORDER BY level DESC;"
  let params = [U.toString appUuid, pkgId, U.toString appUuid]
  logQuery sql params
  let action conn = query conn sql params
  runDB action

findVersionsForPackage :: AppContextC s sc m => String -> String -> m [String]
findVersionsForPackage orgId kmId = do
  appUuid <- asks (.appUuid')
  let sql = fromString "SELECT version FROM package WHERE app_uuid = ? and organization_id = ? and km_id = ?"
  let params = [U.toString appUuid, orgId, kmId]
  logQuery sql params
  let action conn = query conn sql params
  versions <- runDB action
  return . fmap fromOnly $ versions

findPackageById :: AppContextC s sc m => String -> m Package
findPackageById id = do
  appUuid <- asks (.appUuid')
  createFindEntityByFn entityName [appQueryUuid appUuid, ("id", id)]

findPackageById' :: AppContextC s sc m => String -> m (Maybe Package)
findPackageById' id = do
  appUuid <- asks (.appUuid')
  createFindEntityByFn' entityName [appQueryUuid appUuid, ("id", id)]

findPackageWithEventsById :: AppContextC s sc m => String -> m PackageWithEvents
findPackageWithEventsById id = do
  appUuid <- asks (.appUuid')
  createFindEntityByFn entityName [appQueryUuid appUuid, ("id", id)]

countPackages :: AppContextC s sc m => m Int
countPackages = do
  appUuid <- asks (.appUuid')
  createCountByFn entityName appCondition [appUuid]

countPackagesGroupedByOrganizationIdAndKmId :: AppContextC s sc m => m Int
countPackagesGroupedByOrganizationIdAndKmId = do
  appUuid <- asks (.appUuid')
  countPackagesGroupedByOrganizationIdAndKmIdWithApp appUuid

countPackagesGroupedByOrganizationIdAndKmIdWithApp :: AppContextC s sc m => U.UUID -> m Int
countPackagesGroupedByOrganizationIdAndKmIdWithApp appUuid = do
  let sql =
        "SELECT COUNT(*) \
        \FROM (SELECT 1 \
        \      FROM package \
        \      WHERE app_uuid = ? \
        \      GROUP BY organization_id, km_id) nested;"
  let params = [U.toString appUuid]
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
  appUuid <- asks (.appUuid')
  createDeleteEntitiesByFn entityName (appQueryUuid appUuid : queryParams)

deletePackageById :: AppContextC s sc m => String -> m Int64
deletePackageById id = do
  appUuid <- asks (.appUuid')
  createDeleteEntityByFn entityName [appQueryUuid appUuid, ("id", id)]
