module Wizard.Database.DAO.DocumentTemplate.DocumentTemplateDraftDAO where

import Control.Monad.Reader (asks)
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import GHC.Int

import Shared.Database.DAO.DocumentTemplate.DocumentTemplateAssetDAO (deleteAssetsByDocumentTemplateId)
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateFileDAO (deleteFilesByDocumentTemplateId)
import Shared.Database.Mapping.DocumentTemplate.DocumentTemplate ()
import Shared.Model.Common.Page
import Shared.Model.Common.PageMetadata
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Shared.Model.DocumentTemplate.DocumentTemplate
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.DocumentTemplate.DocumentTemplateDraftDataDAO (deleteDraftDataByDocumentTemplateId, deleteDraftDatas)
import Wizard.Database.Mapping.DocumentTemplate.DocumentTemplateDraftList ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.DocumentTemplate.DocumentTemplateDraftList
import Wizard.Util.Logger

entityName = "document_template"

pageLabel = "documentTemplateDrafts"

findDrafts :: AppContextM [DocumentTemplate]
findDrafts = do
  appUuid <- asks currentAppUuid
  createFindEntitiesByFn entityName [appQueryUuid appUuid, ("phase", "DraftDocumentTemplatePhase")]

findDraftsPage :: Maybe String -> Pageable -> [Sort] -> AppContextM (Page DocumentTemplateDraftList)
findDraftsPage mQuery pageable sort =
  -- 1. Prepare variables
  do
    appUuid <- asks currentAppUuid
    let condition = "WHERE phase = 'DraftDocumentTemplatePhase' AND (name ~* ? OR template_id ~* ?) AND app_uuid = ?"
    let conditionParams = [regex mQuery, regex mQuery, U.toString appUuid]
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
              \WHERE phase = 'DraftDocumentTemplatePhase' AND (name ~* ? OR template_id ~* ?) AND app_uuid = ? \
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
  appUuid <- asks currentAppUuid
  createFindEntityByFn entityName [appQueryUuid appUuid, ("id", id), ("phase", "DraftDocumentTemplatePhase")]

countDraftsGroupedByOrganizationIdAndKmId :: AppContextM Int
countDraftsGroupedByOrganizationIdAndKmId = do
  appUuid <- asks currentAppUuid
  countDraftsGroupedByOrganizationIdAndKmIdWithApp appUuid

countDraftsGroupedByOrganizationIdAndKmIdWithApp :: U.UUID -> AppContextM Int
countDraftsGroupedByOrganizationIdAndKmIdWithApp appUuid = do
  let sql =
        "SELECT COUNT(*) \
        \FROM (SELECT 1 \
        \      FROM document_template \
        \      WHERE app_uuid = ? AND phase = 'DraftDocumentTemplatePhase' \
        \      GROUP BY organization_id, template_id) nested;"
  let params = [U.toString appUuid]
  logQuery sql params
  let action conn = query conn sql params
  result <- runDB action
  case result of
    [count] -> return . fromOnly $ count
    _ -> return 0

deleteDrafts :: AppContextM Int64
deleteDrafts = do
  appUuid <- asks currentAppUuid
  deleteDraftDatas
  createDeleteEntitiesByFn entityName [appQueryUuid appUuid, ("phase", "DraftDocumentTemplatePhase")]

deleteDraftByDocumentTemplateId :: String -> AppContextM Int64
deleteDraftByDocumentTemplateId documentTemplateId = do
  appUuid <- asks currentAppUuid
  deleteDraftDataByDocumentTemplateId documentTemplateId
  deleteFilesByDocumentTemplateId documentTemplateId
  deleteAssetsByDocumentTemplateId documentTemplateId
  createDeleteEntitiesByFn entityName [appQueryUuid appUuid, ("id", documentTemplateId), ("phase", "DraftDocumentTemplatePhase")]
