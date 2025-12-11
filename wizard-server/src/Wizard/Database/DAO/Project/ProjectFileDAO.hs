module Wizard.Database.DAO.Project.ProjectFileDAO where

import Control.Monad.Reader (asks)
import Data.String
import Data.Time
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import GHC.Int

import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.PageMetadata
import Shared.Common.Model.Common.Pageable
import Shared.Common.Model.Common.Sort
import Shared.Common.Util.String
import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.Project.File.ProjectFile ()
import Wizard.Database.Mapping.Project.File.ProjectFileList ()
import Wizard.Database.Mapping.Project.File.ProjectFileSimple ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.Project.File.ProjectFile
import Wizard.Model.Project.File.ProjectFileList
import Wizard.Model.Project.File.ProjectFileSimple

entityName = "project_file"

pageLabel = "projectFiles"

findProjectFilesPage :: Maybe String -> Maybe U.UUID -> Pageable -> [Sort] -> AppContextM (Page ProjectFileList)
findProjectFilesPage mQuery mProjectUuid pageable sort = do
  -- 1. Prepare variables
  do
    tenantUuid <- asks currentTenantUuid
    let (queryCondition, queryParam) =
          case mQuery of
            Nothing -> ("", [])
            Just query -> (" AND file_name ~* ?", [query])
    let (projectUuidCondition, projectUuidParam) =
          case mProjectUuid of
            Nothing -> ("", [])
            Just projectUuid -> (" AND project_uuid = ?", [U.toString projectUuid])
    let condition =
          f''
            "WHERE file.tenant_uuid = ? ${queryCondition} ${projectUuidCondition}"
            [ ("queryCondition", queryCondition)
            , ("projectUuidCondition", projectUuidCondition)
            ]
    let conditionParams =
          [U.toString tenantUuid]
            ++ queryParam
            ++ projectUuidParam
    let (sizeI, pageI, skip, limit) = preparePaginationVariables pageable
    -- 2. Get total count
    count <- createCountByFn "project_file file" condition conditionParams
    -- 3. Get entities
    let sql =
          fromString $
            f''
              "SELECT file.uuid, \
              \       file.file_name, \
              \       file.content_type, \
              \       file.file_size, \
              \       file.created_at, \
              \       project.uuid, \
              \       project.name, \
              \       created_by.uuid, \
              \       created_by.first_name, \
              \       created_by.last_name, \
              \       created_by.email, \
              \       created_by.image_url \
              \FROM project_file file \
              \LEFT JOIN user_entity created_by ON created_by.uuid = file.created_by AND created_by.tenant_uuid = file.tenant_uuid \
              \LEFT JOIN project ON project.uuid = file.project_uuid AND project.tenant_uuid = file.tenant_uuid \
              \${condition} \
              \${sort} \
              \OFFSET ${offset} \
              \LIMIT ${limit}"
              [ ("condition", condition)
              , ("sort", mapSort sort)
              , ("offset", show skip)
              , ("limit", show sizeI)
              ]
    logQuery sql conditionParams
    let action conn = query conn sql conditionParams
    entities <- runDB action
    -- 4. Constructor response
    let metadata =
          PageMetadata
            { size = sizeI
            , totalElements = count
            , totalPages = computeTotalPage count sizeI
            , number = pageI
            }
    return $ Page pageLabel metadata entities

findProjectFilesByProject :: U.UUID -> AppContextM [ProjectFile]
findProjectFilesByProject projectUuid = do
  tenantUuid <- asks currentTenantUuid
  createFindEntitiesWithFieldsByFn "*" entityName [tenantQueryUuid tenantUuid, ("project_uuid", U.toString projectUuid)]

findProjectFilesSimpleByProject :: U.UUID -> AppContextM [ProjectFileSimple]
findProjectFilesSimpleByProject projectUuid = do
  tenantUuid <- asks currentTenantUuid
  createFindEntitiesWithFieldsByFn "uuid, file_name, content_type, file_size" entityName [tenantQueryUuid tenantUuid, ("project_uuid", U.toString projectUuid)]

findProjectFileByUuid :: U.UUID -> AppContextM ProjectFile
findProjectFileByUuid uuid = do
  tenantUuid <- asks currentTenantUuid
  createFindEntityByFn entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]

sumProjectFileSize :: AppContextM Int64
sumProjectFileSize = do
  tenantUuid <- asks currentTenantUuid
  sumProjectFileSizeWithTenant tenantUuid

sumProjectFileSizeWithTenant :: U.UUID -> AppContextM Int64
sumProjectFileSizeWithTenant tenantUuid = createSumByFn entityName "file_size" tenantCondition [U.toString tenantUuid]

insertProjectFile :: ProjectFile -> AppContextM Int64
insertProjectFile = createInsertFn entityName

deleteProjectFiles :: AppContextM Int64
deleteProjectFiles = createDeleteEntitiesFn entityName

deleteProjectFilesNewerThen :: U.UUID -> UTCTime -> AppContextM Int64
deleteProjectFilesNewerThen projectUuid timestamp = do
  tenantUuid <- asks currentTenantUuid
  let sql =
        fromString
          "DELETE FROM project_file \
          \WHERE tenant_uuid = ? \
          \  AND project_uuid = ? \
          \  AND created_at > ?"
  let params = [U.toString tenantUuid, U.toString projectUuid, show timestamp]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteProjectFileByUuid :: U.UUID -> AppContextM Int64
deleteProjectFileByUuid uuid = do
  tenantUuid <- asks currentTenantUuid
  createDeleteEntityByFn entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]
