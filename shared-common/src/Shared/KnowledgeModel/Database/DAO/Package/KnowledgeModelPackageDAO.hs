module Shared.KnowledgeModel.Database.DAO.Package.KnowledgeModelPackageDAO where

import Control.Monad.Reader (asks)
import Data.String (fromString)
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import GHC.Int

import Shared.Common.Database.DAO.Common
import Shared.Common.Model.Context.AppContext
import Shared.KnowledgeModel.Database.Mapping.KnowledgeModel.Package.KnowledgeModelPackage ()
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage

entityName = "knowledge_model_package"

findPackages :: AppContextC s sc m => m [KnowledgeModelPackage]
findPackages = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid]

findPackagesFiltered :: AppContextC s sc m => [(String, String)] -> m [KnowledgeModelPackage]
findPackagesFiltered queryParams = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesByFn entityName (tenantQueryUuid tenantUuid : queryParams)

findPackagesByOrganizationIdAndKmId :: AppContextC s sc m => String -> String -> m [KnowledgeModelPackage]
findPackagesByOrganizationIdAndKmId organizationId kmId = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid, ("organization_id", organizationId), ("km_id", kmId)]

findPackagesByPreviousPackageId :: AppContextC s sc m => String -> m [KnowledgeModelPackage]
findPackagesByPreviousPackageId previousPackageId = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid, ("previous_package_id", previousPackageId)]

findPackagesByForkOfPackageId :: AppContextC s sc m => String -> m [KnowledgeModelPackage]
findPackagesByForkOfPackageId forkOfPackageId = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid, ("fork_of_package_id", forkOfPackageId)]

findPackagesByUnsupportedMetamodelVersion :: AppContextC s sc m => Int -> m [KnowledgeModelPackage]
findPackagesByUnsupportedMetamodelVersion metamodelVersion = do
  tenantUuid <- asks (.tenantUuid')
  let sql = fromString "SELECT * FROM knowledge_model_package WHERE metamodel_version != ? AND tenant_uuid = ?"
  let params = [toField metamodelVersion, toField tenantUuid]
  logQuery sql params
  let action conn = query conn sql params
  runDB action

findSeriesOfPackagesRecursiveById :: (AppContextC s sc m, FromRow packageWithEvents) => String -> m [packageWithEvents]
findSeriesOfPackagesRecursiveById pkgId = do
  tenantUuid <- asks (.tenantUuid')
  let sql =
        fromString
          "WITH RECURSIVE recursive AS ( \
          \  SELECT *, 1 as level \
          \  FROM knowledge_model_package \
          \  WHERE tenant_uuid = ? AND id = ? \
          \  UNION ALL \
          \  SELECT pkg.*, level + 1 as level \
          \  FROM knowledge_model_package pkg \
          \  INNER JOIN recursive r ON pkg.id = r.previous_package_id AND pkg.tenant_uuid = ? \
          \) \
          \SELECT id, \
          \       name, \
          \       organization_id, \
          \       km_id, \
          \       version, \
          \       phase, \
          \       metamodel_version, \
          \       description, \
          \       readme, \
          \       license, \
          \       previous_package_id, \
          \       fork_of_package_id, \
          \       merge_checkpoint_package_id, \
          \       (SELECT coalesce(jsonb_agg(jsonb_build_object( \
          \                        'uuid', pkg_event.uuid, \
          \                        'parentUuid', pkg_event.parent_uuid, \
          \                        'entityUuid', pkg_event.entity_uuid, \
          \                        'content', pkg_event.content, \
          \                        'createdAt', to_char(pkg_event.created_at AT TIME ZONE 'UTC', 'YYYY-MM-DD\"T\"HH24:MI:SS\"Z\"') \
          \               ) \
          \           ), '[]'::jsonb) \
          \               FROM (SELECT * \
          \                     FROM knowledge_model_package_event \
          \                     WHERE knowledge_model_package_event.tenant_uuid = recursive.tenant_uuid \
          \                       AND knowledge_model_package_event.package_id = recursive.id \
          \                     ORDER BY knowledge_model_package_event.created_at) pkg_event), \
          \       non_editable, \
          \       created_at \
          \FROM recursive \
          \ORDER BY level DESC;"
  let params = [U.toString tenantUuid, pkgId, U.toString tenantUuid]
  logQuery sql params
  let action conn = query conn sql params
  runDB action

findVersionsForPackage :: AppContextC s sc m => String -> String -> m [String]
findVersionsForPackage orgId kmId = do
  tenantUuid <- asks (.tenantUuid')
  let sql = fromString "SELECT version FROM knowledge_model_package WHERE tenant_uuid = ? and organization_id = ? and km_id = ?"
  let params = [U.toString tenantUuid, orgId, kmId]
  logQuery sql params
  let action conn = query conn sql params
  versions <- runDB action
  return . fmap fromOnly $ versions

findPackageById :: AppContextC s sc m => String -> m KnowledgeModelPackage
findPackageById id = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn entityName [tenantQueryUuid tenantUuid, ("id", id)]

findPackageById' :: AppContextC s sc m => String -> m (Maybe KnowledgeModelPackage)
findPackageById' id = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn' entityName [tenantQueryUuid tenantUuid, ("id", id)]

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
        \      FROM knowledge_model_package \
        \      WHERE tenant_uuid = ? \
        \      GROUP BY organization_id, km_id) nested;"
  let params = [U.toString tenantUuid]
  logQuery sql params
  let action conn = query conn sql params
  result <- runDB action
  case result of
    [count] -> return . fromOnly $ count
    _ -> return 0

insertPackage :: AppContextC s sc m => KnowledgeModelPackage -> m Int64
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
