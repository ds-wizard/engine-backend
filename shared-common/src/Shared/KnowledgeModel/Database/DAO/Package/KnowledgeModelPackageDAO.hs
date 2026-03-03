module Shared.KnowledgeModel.Database.DAO.Package.KnowledgeModelPackageDAO where

import Control.Monad.Reader (asks)
import Data.String (fromString)
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import GHC.Int

import Shared.Common.Database.DAO.Common
import Shared.Common.Model.Context.AppContext
import Shared.Common.Util.String (f'')
import Shared.Coordinate.Model.Coordinate.Coordinate
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

findPackagesByPreviousPackageUuid :: AppContextC s sc m => U.UUID -> m [KnowledgeModelPackage]
findPackagesByPreviousPackageUuid previousPackageUuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid, ("previous_package_uuid", U.toString previousPackageUuid)]

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

findSeriesOfPackagesRecursiveByUuid :: (AppContextC s sc m, FromRow packageWithEvents) => U.UUID -> m [packageWithEvents]
findSeriesOfPackagesRecursiveByUuid pkgUuid = do
  tenantUuid <- asks (.tenantUuid')
  let sql =
        fromString
          "WITH RECURSIVE recursive AS ( \
          \  SELECT *, 1 as level \
          \  FROM knowledge_model_package \
          \  WHERE tenant_uuid = ? AND uuid = ? \
          \  UNION ALL \
          \  SELECT pkg.*, level + 1 as level \
          \  FROM knowledge_model_package pkg \
          \  INNER JOIN recursive r ON pkg.uuid = r.previous_package_uuid AND pkg.tenant_uuid = ? \
          \) \
          \SELECT concat(organization_id, ':', km_id, ':', version) AS id, \
          \       name, \
          \       organization_id, \
          \       km_id, \
          \       version, \
          \       phase, \
          \       metamodel_version, \
          \       description, \
          \       readme, \
          \       license, \
          \       (SELECT concat(pp.organization_id, ':', pp.km_id, ':', pp.version) FROM knowledge_model_package pp WHERE pp.tenant_uuid = recursive.tenant_uuid AND pp.uuid = recursive.previous_package_uuid) AS previous_package_uuid, \
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
          \                       AND knowledge_model_package_event.package_uuid = recursive.uuid \
          \                     ORDER BY knowledge_model_package_event.created_at) pkg_event), \
          \       non_editable, \
          \       created_at \
          \FROM recursive \
          \ORDER BY level DESC;"
  let params = [U.toString tenantUuid, U.toString pkgUuid, U.toString tenantUuid]
  logQuery sql params
  let action conn = query conn sql params
  runDB action

findUsablePackagesForDocumentTemplate :: (AppContextC s sc m, FromRow KnowledgeModelPackage) => U.UUID -> m [KnowledgeModelPackage]
findUsablePackagesForDocumentTemplate dtUuid = do
  tenantUuid <- asks (.tenantUuid')
  let sql =
        fromString
          "WITH expanded_rules AS (SELECT tenant_uuid, \
          \                               jsonb_array_elements(allowed_packages) AS rule \
          \                        FROM document_template \
          \                        WHERE uuid = ? \
          \                          AND tenant_uuid = ?) \
          \SELECT DISTINCT ON (kmp.organization_id, kmp.km_id) kmp.* \
          \FROM knowledge_model_package kmp \
          \     JOIN expanded_rules er ON \
          \    kmp.tenant_uuid = er.tenant_uuid \
          \        AND (er.rule ->> 'orgId' IS NULL OR kmp.organization_id = er.rule ->> 'orgId') \
          \        AND (er.rule ->> 'kmId' IS NULL OR kmp.km_id = er.rule ->> 'kmId') \
          \        AND (er.rule ->> 'minVersion' IS NULL OR \
          \             string_to_array(kmp.version, '.')::int[] >= string_to_array(er.rule ->> 'minVersion', '.')::int[]) \
          \        AND (er.rule ->> 'maxVersion' IS NULL OR \
          \             string_to_array(kmp.version, '.')::int[] <= string_to_array(er.rule ->> 'maxVersion', '.')::int[]) \
          \ORDER BY kmp.organization_id, \
          \         kmp.km_id, \
          \         string_to_array(kmp.version, '.')::int[] DESC;"
  let params = [U.toString dtUuid, U.toString tenantUuid]
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

findPackageByUuid :: AppContextC s sc m => U.UUID -> m KnowledgeModelPackage
findPackageByUuid uuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]

findPackageByUuid' :: AppContextC s sc m => U.UUID -> m (Maybe KnowledgeModelPackage)
findPackageByUuid' uuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn' entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]

findPackageByCoordinate :: AppContextC s sc m => Coordinate -> m KnowledgeModelPackage
findPackageByCoordinate Coordinate {..} = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn entityName [tenantQueryUuid tenantUuid, ("organization_id", organizationId), ("km_id", entityId), ("version", version)]

findPackageByCoordinate' :: AppContextC s sc m => Coordinate -> m (Maybe KnowledgeModelPackage)
findPackageByCoordinate' Coordinate {..} = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn' entityName [tenantQueryUuid tenantUuid, ("organization_id", organizationId), ("km_id", entityId), ("version", version)]

findLatestPackageByOrganizationIdAndKmId :: AppContextC s sc m => String -> String -> Maybe KnowledgeModelPackagePhase -> m KnowledgeModelPackage
findLatestPackageByOrganizationIdAndKmId orgId kmId mPhase = do
  (action, phaseParams) <- createFindLatestPackageByOrganizationIdAndKmIdAction orgId kmId mPhase
  runOneEntityDB entityName action ([("organization_id", orgId), ("km_id", kmId)] ++ phaseParams)

findLatestPackageByOrganizationIdAndKmId' :: AppContextC s sc m => String -> String -> Maybe KnowledgeModelPackagePhase -> m (Maybe KnowledgeModelPackage)
findLatestPackageByOrganizationIdAndKmId' orgId kmId mPhase = do
  (action, phaseParams) <- createFindLatestPackageByOrganizationIdAndKmIdAction orgId kmId mPhase
  runOneEntityDB' entityName action ([("organization_id", orgId), ("km_id", kmId)] ++ phaseParams)

createFindLatestPackageByOrganizationIdAndKmIdAction :: AppContextC s sc m => String -> String -> Maybe KnowledgeModelPackagePhase -> m (Connection -> IO [KnowledgeModelPackage], [(String, String)])
createFindLatestPackageByOrganizationIdAndKmIdAction orgId kmId mPhase = do
  tenantUuid <- asks (.tenantUuid')
  let (phaseCondition, phaseParams) =
        case mPhase of
          Just ReleasedKnowledgeModelPackagePhase -> ("AND phase = 'ReleasedKnowledgeModelPackagePhase'", [("phase", "ReleasedKnowledgeModelPackagePhase")])
          Just DeprecatedKnowledgeModelPackagePhase -> ("AND phase = 'DeprecatedKnowledgeModelPackagePhase'", [("phase", "DeprecatedKnowledgeModelPackagePhase")])
          Nothing -> ("", [])
  let sql =
        fromString $
          f''
            "SELECT * \
            \FROM knowledge_model_package \
            \WHERE tenant_uuid = ? \
            \  AND organization_id = ? \
            \  AND km_id = ? \
            \  ${phaseCondition} \
            \ORDER BY split_part(version, '.', 1)::int DESC, \
            \        split_part(version, '.', 2)::int DESC, \
            \        split_part(version, '.', 3)::int DESC \
            \LIMIT 1"
            [("phaseCondition", phaseCondition)]
  let params = [U.toString tenantUuid, orgId, kmId]
  logQuery sql params
  return (\conn -> query conn sql params, phaseParams)

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

deletePackageByUuid :: AppContextC s sc m => U.UUID -> m Int64
deletePackageByUuid uuid = do
  tenantUuid <- asks (.tenantUuid')
  createDeleteEntityByFn entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]

deletePackageByCoordinate :: AppContextC s sc m => Coordinate -> m Int64
deletePackageByCoordinate Coordinate {..} = do
  tenantUuid <- asks (.tenantUuid')
  createDeleteEntityByFn entityName [tenantQueryUuid tenantUuid, ("organization_id", organizationId), ("km_id", entityId), ("version", version)]
