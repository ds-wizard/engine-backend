module Wizard.Database.DAO.Project.ProjectVersionDAO where

import Control.Monad (unless, void)
import Control.Monad.Reader (asks)
import Data.String (fromString)
import Data.Time
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Common.Util.Logger
import Shared.Common.Util.String
import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.Project.Version.ProjectVersion ()
import Wizard.Database.Mapping.Project.Version.ProjectVersionList ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.Project.Version.ProjectVersion
import Wizard.Model.Project.Version.ProjectVersionList

entityName = "project_version"

pageLabel = "projectVersions"

findProjectVersionsByProjectUuid :: U.UUID -> AppContextM [ProjectVersion]
findProjectVersionsByProjectUuid projectUuid = do
  tenantUuid <- asks currentTenantUuid
  createFindEntitiesWithFieldsByFn "*" entityName [tenantQueryUuid tenantUuid, ("project_uuid", U.toString projectUuid)]

findProjectVersionListByProjectUuidAndCreatedAt :: U.UUID -> Maybe UTCTime -> AppContextM [ProjectVersionList]
findProjectVersionListByProjectUuidAndCreatedAt projectUuid mCreatedAt = do
  tenantUuid <- asks currentTenantUuid
  let (createdAtCondition, createdAtParams) =
        case mCreatedAt of
          Just createdAt -> ("AND v.created_at <= ?", [toField createdAt])
          Nothing -> ("", [])
  let sql =
        fromString $
          f''
            "SELECT v.uuid, \
            \       v.name, \
            \       v.description, \
            \       v.event_uuid, \
            \       v.created_at, \
            \       v.updated_at, \
            \       u.uuid, \
            \       u.first_name, \
            \       u.last_name, \
            \       u.email, \
            \       u.image_url \
            \FROM project_version v \
            \JOIN user_entity u ON u.uuid = v.created_by AND u.tenant_uuid = v.tenant_uuid ${createdAtCondition} \
            \WHERE v.tenant_uuid = ? AND v.project_uuid = ? \
            \ORDER BY v.created_at"
            [("createdAtCondition", createdAtCondition)]
  let params = createdAtParams ++ [toField tenantUuid, toField projectUuid]
  logInfoI _CMP_DATABASE sql
  let action conn = query conn (fromString sql) params
  runDB action

findProjectVersionByUuid :: U.UUID -> AppContextM ProjectVersion
findProjectVersionByUuid uuid = do
  tenantUuid <- asks currentTenantUuid
  createFindEntityWithFieldsByFn "*" False entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]

findProjectVersionByEventUuid' :: U.UUID -> U.UUID -> AppContextM (Maybe ProjectVersion)
findProjectVersionByEventUuid' projectUuid eventUuid = do
  tenantUuid <- asks currentTenantUuid
  createFindEntityByFn' entityName [tenantQueryUuid tenantUuid, ("project_uuid", U.toString projectUuid), ("event_uuid", U.toString eventUuid)]

insertProjectVersion :: ProjectVersion -> AppContextM Int64
insertProjectVersion = createInsertFn entityName

updateProjectVersionByUuid :: ProjectVersion -> AppContextM Int64
updateProjectVersionByUuid version = do
  tenantUuid <- asks currentTenantUuid
  let sql = fromString "UPDATE project_version SET uuid = ?, name = ?, description = ?, event_uuid = ?, project_uuid = ?, tenant_uuid = ?, created_by = ?, created_at = ?, updated_at = ? WHERE uuid = ? AND tenant_uuid = ?"
  let params = toRow version ++ [toField version.uuid, toField tenantUuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteProjectVersions :: AppContextM Int64
deleteProjectVersions = createDeleteEntitiesFn entityName

deleteProjectVersionsByUuids :: [U.UUID] -> AppContextM ()
deleteProjectVersionsByUuids versionUuids =
  unless
    (null versionUuids)
    (void $ createDeleteEntityWhereInFn entityName "uuid" (fmap U.toString versionUuids))

deleteProjectVersionByUuid :: U.UUID -> AppContextM Int64
deleteProjectVersionByUuid uuid = do
  tenantUuid <- asks currentTenantUuid
  createDeleteEntitiesByFn entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]
