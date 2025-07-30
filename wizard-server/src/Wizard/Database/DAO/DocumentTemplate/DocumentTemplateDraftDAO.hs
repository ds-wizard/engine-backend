module Wizard.Database.DAO.DocumentTemplate.DocumentTemplateDraftDAO where

import Control.Monad.Reader (asks)
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import GHC.Int

import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.PageMetadata
import Shared.Common.Model.Common.Pageable
import Shared.Common.Model.Common.Sort
import Shared.Common.Util.Logger
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.DocumentTemplate.DocumentTemplateDraftDataDAO (deleteDraftData, deleteDraftDataByDocumentTemplateId)
import Wizard.Database.Mapping.DocumentTemplate.DocumentTemplateDraftList ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.DocumentTemplate.DocumentTemplateDraftList
import WizardLib.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateAssetDAO (deleteAssetsByDocumentTemplateId)
import WizardLib.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateFileDAO (deleteFilesByDocumentTemplateId)
import WizardLib.DocumentTemplate.Database.Mapping.DocumentTemplate.DocumentTemplate ()
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate

entityName = "document_template"

pageLabel = "documentTemplateDrafts"

findDrafts :: AppContextM [DocumentTemplate]
findDrafts = do
  tenantUuid <- asks currentTenantUuid
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid, ("phase", "DraftDocumentTemplatePhase")]

findDraftsPage :: Maybe String -> Pageable -> [Sort] -> AppContextM (Page DocumentTemplateDraftList)
findDraftsPage mQuery pageable sort =
  -- 1. Prepare variables
  do
    tenantUuid <- asks currentTenantUuid
    let condition = "WHERE phase = 'DraftDocumentTemplatePhase' AND (name ~* ? OR template_id ~* ?) AND tenant_uuid = ?"
    let conditionParams = [regexM mQuery, regexM mQuery, U.toString tenantUuid]
    let (sizeI, pageI, skip, limit) = preparePaginationVariables pageable
    -- 2. Get total count
    count <- createCountByFn entityName condition conditionParams
    -- 3. Get entities
    let sql =
          fromString $
            f'
              "SELECT id, \
              \       name, \
              \       organization_id, \
              \       template_id, \
              \       version, \
              \       description, \
              \       created_at, \
              \       updated_at \
              \FROM document_template \
              \WHERE phase = 'DraftDocumentTemplatePhase' AND (name ~* ? OR template_id ~* ?) AND tenant_uuid = ? \
              \%s OFFSET %s LIMIT %s"
              [mapSort sort, show skip, show sizeI]
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

findDraftById :: String -> AppContextM DocumentTemplate
findDraftById id = do
  tenantUuid <- asks currentTenantUuid
  createFindEntityByFn entityName [tenantQueryUuid tenantUuid, ("id", id), ("phase", "DraftDocumentTemplatePhase")]

countDraftsGroupedByOrganizationIdAndKmId :: AppContextM Int
countDraftsGroupedByOrganizationIdAndKmId = do
  tenantUuid <- asks currentTenantUuid
  countDraftsGroupedByOrganizationIdAndKmIdWithTenant tenantUuid

countDraftsGroupedByOrganizationIdAndKmIdWithTenant :: U.UUID -> AppContextM Int
countDraftsGroupedByOrganizationIdAndKmIdWithTenant tenantUuid = do
  let sql =
        "SELECT COUNT(*) \
        \FROM (SELECT 1 \
        \      FROM document_template \
        \      WHERE tenant_uuid = ? AND phase = 'DraftDocumentTemplatePhase' \
        \      GROUP BY organization_id, template_id) nested;"
  let params = [U.toString tenantUuid]
  logQuery sql params
  let action conn = query conn sql params
  result <- runDB action
  case result of
    [count] -> return . fromOnly $ count
    _ -> return 0

moveFolder :: String -> String -> String -> AppContextM Int64
moveFolder documentTemplateId currentFolder newFolder = do
  tenantUuid <- asks currentTenantUuid
  let sql =
        fromString
          "UPDATE document_template_asset \
          \SET file_name = concat(?, substr(file_name, length(?) + 1)) \
          \WHERE tenant_uuid = ?  \
          \  AND document_template_id = ? \
          \  AND starts_with(file_name, ?); \
          \ \
          \UPDATE document_template_file \
          \SET file_name = concat(?, substr(file_name, length(?) + 1)) \
          \WHERE tenant_uuid = ?  \
          \  AND document_template_id = ? \
          \  AND starts_with(file_name, ?);"
  let paramsForOneUpdate =
        [ toField newFolder
        , toField currentFolder
        , toField tenantUuid
        , toField documentTemplateId
        , toField currentFolder
        ]
  let params = paramsForOneUpdate ++ paramsForOneUpdate
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteFolder :: String -> String -> AppContextM Int64
deleteFolder documentTemplateId path = do
  tenantUuid <- asks currentTenantUuid
  let sql =
        fromString
          "DELETE FROM document_template_asset \
          \WHERE tenant_uuid = ?  \
          \  AND document_template_id = ? \
          \  AND starts_with(file_name, ?); \
          \ \
          \DELETE FROM document_template_file \
          \WHERE tenant_uuid = ?  \
          \  AND document_template_id = ? \
          \  AND starts_with(file_name, ?);"
  let paramsForOneUpdate =
        [ toField tenantUuid
        , toField documentTemplateId
        , toField path
        ]
  let params = paramsForOneUpdate ++ paramsForOneUpdate
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteDrafts :: AppContextM Int64
deleteDrafts = do
  tenantUuid <- asks currentTenantUuid
  deleteDraftData
  createDeleteEntitiesByFn entityName [tenantQueryUuid tenantUuid, ("phase", "DraftDocumentTemplatePhase")]

deleteDraftByDocumentTemplateId :: String -> AppContextM Int64
deleteDraftByDocumentTemplateId documentTemplateId = do
  tenantUuid <- asks currentTenantUuid
  deleteDraftDataByDocumentTemplateId documentTemplateId
  deleteFilesByDocumentTemplateId documentTemplateId
  deleteAssetsByDocumentTemplateId documentTemplateId
  createDeleteEntitiesByFn entityName [tenantQueryUuid tenantUuid, ("id", documentTemplateId), ("phase", "DraftDocumentTemplatePhase")]
