module Wizard.Database.DAO.QuestionnaireImporter.QuestionnaireImporterDAO where

import Control.Lens ((^.))
import Control.Monad.Reader (asks)
import Data.String (fromString)
import Data.Time
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import LensesConfig
import Shared.Database.DAO.Common hiding (runInTransaction)
import Shared.Model.Common.Page
import Shared.Model.Common.PageMetadata
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Shared.Util.String
import Wizard.Database.Mapping.QuestionnaireImporter.QuestionnaireImporter ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.QuestionnaireImporter.QuestionnaireImporter
import Wizard.Util.Logger

entityName = "questionnaire_importer"

pageLabel = "questionnaireImporters"

findQuestionnaireImportersPage ::
     Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe Bool
  -> Pageable
  -> [Sort]
  -> AppContextM (Page QuestionnaireImporter)
findQuestionnaireImportersPage mOrganizationId mImporterId mQuery mEnabled pageable sort =
  createFindEntitiesGroupByCoordinatePageableQuerySortFn
    entityName
    pageLabel
    pageable
    sort
    "*"
    "importer_id"
    mQuery
    mEnabled
    mOrganizationId
    mImporterId

findQuestionnaireImporterById :: String -> AppContextM QuestionnaireImporter
findQuestionnaireImporterById qiId = do
  appUuid <- asks _appContextAppUuid
  createFindEntityByFn entityName [appQueryUuid appUuid, ("id", qiId)]

insertQuestionnaireImporter :: QuestionnaireImporter -> AppContextM Int64
insertQuestionnaireImporter = createInsertFn entityName

updateQuestionnaireImporterById :: QuestionnaireImporter -> AppContextM Int64
updateQuestionnaireImporterById importer = do
  appUuid <- asks _appContextAppUuid
  let sql =
        fromString
          "UPDATE questionnaire_importer SET id = ?, name = ?, organization_id = ?, importer_id = ?, version = ?, metamodel_version = ?, description = ?, readme = ?, license = ?, allowed_packages = ?, url = ?, enabled = ?, app_uuid = ?, created_at = ?, updated_at = ? WHERE app_uuid = ? AND id = ?"
  let params = toRow importer ++ [toField appUuid, toField $ importer ^. qiId]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

updateQuestionnaireImporterPasswordById :: String -> Bool -> UTCTime -> AppContextM Int64
updateQuestionnaireImporterPasswordById qiId enabled uUpdatedAt = do
  appUuid <- asks _appContextAppUuid
  let sql = fromString "UPDATE questionnaire_importer SET enabled = ?, updated_at = ? WHERE app_uuid = ? AND uuid = ?"
  let params = [toField enabled, toField uUpdatedAt, toField appUuid, toField qiId]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteQuestionnaireImporters :: AppContextM Int64
deleteQuestionnaireImporters = createDeleteEntitiesFn entityName

deleteQuestionnaireImporterById :: String -> AppContextM Int64
deleteQuestionnaireImporterById qiId = do
  appUuid <- asks _appContextAppUuid
  createDeleteEntityByFn entityName [appQueryUuid appUuid, ("id", qiId)]

-- --------------------------------
-- PRIVATE
-- --------------------------------
createFindEntitiesGroupByCoordinatePageableQuerySortFn entityName pageLabel pageable sort fields entityId mQuery mEnabled mOrganizationId mEntityId
  -- 1. Prepare variables
 = do
  appUuid <- asks _appContextAppUuid
  let (sizeI, pageI, skip, limit) = preparePaginationVariables pageable
  let enabledCondition =
        case mEnabled of
          Just True -> "enabled = true AND"
          Just False -> "enabled = false AND"
          _ -> ""
  -- 2. Get total count
  count <- createCountGroupByCoordinateFn entityName entityId mQuery enabledCondition mOrganizationId mEntityId
  -- 3. Get entities
  let sql =
        f'
          "SELECT %s \
           \FROM %s \
           \WHERE app_uuid = ? AND id IN ( \
           \    SELECT CONCAT(organization_id, ':', %s, ':', (max(string_to_array(version, '.')::int[]))[1] || '.' || \
           \                                                    (max(string_to_array(version, '.')::int[]))[2] || '.' || \
           \                                                    (max(string_to_array(version, '.')::int[]))[3]) \
           \    FROM %s \
           \    WHERE %s app_uuid = ? AND (name ~* ? OR id ~* ?) %s \
           \    GROUP BY organization_id, %s \
           \) \
           \%s \
           \offset %s \
           \limit %s"
          [ fields
          , entityName
          , entityId
          , entityName
          , enabledCondition
          , mapToDBCoordinatesSql entityName entityId mOrganizationId mEntityId
          , entityId
          , mapSort sort
          , show skip
          , show sizeI
          ]
  logInfo _CMP_DATABASE sql
  let action conn =
        query
          conn
          (fromString sql)
          (U.toString appUuid :
           U.toString appUuid : regex mQuery : regex mQuery : mapToDBCoordinatesParams mOrganizationId mEntityId)
  entities <- runDB action
  -- 4. Constructor response
  let metadata =
        PageMetadata
          { _pageMetadataSize = sizeI
          , _pageMetadataTotalElements = count
          , _pageMetadataTotalPages = computeTotalPage count sizeI
          , _pageMetadataNumber = pageI
          }
  return $ Page pageLabel metadata entities

createCountGroupByCoordinateFn ::
     String -> String -> Maybe String -> String -> Maybe String -> Maybe String -> AppContextM Int
createCountGroupByCoordinateFn entityName entityId mQuery enabledCondition mOrganizationId mEntityId = do
  appUuid <- asks _appContextAppUuid
  let sql =
        f'
          "SELECT COUNT(*) \
          \ FROM (SELECT COUNT(*) \
          \   FROM %s \
          \   WHERE %s app_uuid = ? AND (name ~* ? OR id ~* ?) %s \
          \   GROUP BY organization_id, %s) p"
          [entityName, enabledCondition, mapToDBCoordinatesSql entityName entityId mOrganizationId mEntityId, entityId]
  logInfo _CMP_DATABASE sql
  let action conn =
        query
          conn
          (fromString sql)
          (U.toString appUuid : regex mQuery : regex mQuery : mapToDBCoordinatesParams mOrganizationId mEntityId)
  result <- runDB action
  case result of
    [count] -> return . fromOnly $ count
    _ -> return 0
