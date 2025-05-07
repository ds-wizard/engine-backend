module Wizard.Database.DAO.Questionnaire.QuestionnaireFileDAO where

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
import Wizard.Database.Mapping.Questionnaire.QuestionnaireFile ()
import Wizard.Database.Mapping.Questionnaire.QuestionnaireFileList ()
import Wizard.Database.Mapping.Questionnaire.QuestionnaireFileSimple ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.Questionnaire.QuestionnaireFile
import Wizard.Model.Questionnaire.QuestionnaireFileList
import Wizard.Model.Questionnaire.QuestionnaireFileSimple

entityName = "questionnaire_file"

pageLabel = "questionnaireFiles"

findQuestionnaireFilesPage :: Maybe String -> Maybe U.UUID -> Pageable -> [Sort] -> AppContextM (Page QuestionnaireFileList)
findQuestionnaireFilesPage mQuery mQtnUuid pageable sort = do
  -- 1. Prepare variables
  do
    tenantUuid <- asks currentTenantUuid
    let (queryCondition, queryParam) =
          case mQuery of
            Nothing -> ("", [])
            Just query -> (" AND file_name ~* ?", [query])
    let (qtnUuidCondition, qtnUuidParam) =
          case mQtnUuid of
            Nothing -> ("", [])
            Just qtnUuid -> (" AND questionnaire_uuid = ?", [U.toString qtnUuid])
    let condition =
          f''
            "WHERE file.tenant_uuid = ? ${queryCondition} ${qtnUuidCondition}"
            [ ("queryCondition", queryCondition)
            , ("qtnUuidCondition", qtnUuidCondition)
            ]
    let conditionParams =
          [U.toString tenantUuid]
            ++ queryParam
            ++ qtnUuidParam
    let (sizeI, pageI, skip, limit) = preparePaginationVariables pageable
    -- 2. Get total count
    count <- createCountByFn "questionnaire_file file" condition conditionParams
    -- 3. Get entities
    let sql =
          fromString $
            f''
              "SELECT file.uuid, \
              \       file.file_name, \
              \       file.content_type, \
              \       file.file_size, \
              \       file.created_at, \
              \       questionnaire.uuid, \
              \       questionnaire.name, \
              \       created_by.uuid, \
              \       created_by.first_name, \
              \       created_by.last_name, \
              \       created_by.email, \
              \       created_by.image_url \
              \FROM questionnaire_file file \
              \LEFT JOIN user_entity created_by ON created_by.uuid = file.created_by AND created_by.tenant_uuid = file.tenant_uuid \
              \LEFT JOIN questionnaire ON questionnaire.uuid = file.questionnaire_uuid AND questionnaire.tenant_uuid = file.tenant_uuid \
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

findQuestionnaireFilesByQuestionnaire :: U.UUID -> AppContextM [QuestionnaireFile]
findQuestionnaireFilesByQuestionnaire qtnUuid = do
  tenantUuid <- asks currentTenantUuid
  createFindEntitiesWithFieldsByFn "*" entityName [tenantQueryUuid tenantUuid, ("questionnaire_uuid", U.toString qtnUuid)]

findQuestionnaireFilesSimpleByQuestionnaire :: U.UUID -> AppContextM [QuestionnaireFileSimple]
findQuestionnaireFilesSimpleByQuestionnaire qtnUuid = do
  tenantUuid <- asks currentTenantUuid
  createFindEntitiesWithFieldsByFn "uuid, file_name, content_type, file_size" entityName [tenantQueryUuid tenantUuid, ("questionnaire_uuid", U.toString qtnUuid)]

clearQuestionnaireFileCreatedBy :: U.UUID -> AppContextM ()
clearQuestionnaireFileCreatedBy userUuid = do
  let sql = fromString "UPDATE questionnaire_file SET created_by = null WHERE created_by = ?"
  let params = [U.toString userUuid]
  logInsertAndUpdate sql params
  let action conn = execute conn sql params
  runDB action
  return ()

findQuestionnaireFileByUuid :: U.UUID -> AppContextM QuestionnaireFile
findQuestionnaireFileByUuid uuid = do
  tenantUuid <- asks currentTenantUuid
  createFindEntityByFn entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]

sumQuestionnaireFileSize :: AppContextM Int64
sumQuestionnaireFileSize = do
  tenantUuid <- asks currentTenantUuid
  sumQuestionnaireFileSizeWithTenant tenantUuid

sumQuestionnaireFileSizeWithTenant :: U.UUID -> AppContextM Int64
sumQuestionnaireFileSizeWithTenant tenantUuid = createSumByFn entityName "file_size" tenantCondition [U.toString tenantUuid]

insertQuestionnaireFile :: QuestionnaireFile -> AppContextM Int64
insertQuestionnaireFile = createInsertFn entityName

deleteQuestionnaireFiles :: AppContextM Int64
deleteQuestionnaireFiles = createDeleteEntitiesFn entityName

deleteQuestionnaireFilesNewerThen :: U.UUID -> UTCTime -> AppContextM Int64
deleteQuestionnaireFilesNewerThen qtnUuid timestamp = do
  tenantUuid <- asks currentTenantUuid
  let sql =
        fromString
          "DELETE FROM questionnaire_file \
          \WHERE tenant_uuid = ? \
          \  AND questionnaire_uuid = ? \
          \  AND created_at > ?"
  let params = [U.toString tenantUuid, U.toString qtnUuid, show timestamp]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteQuestionnaireFileByUuid :: U.UUID -> AppContextM Int64
deleteQuestionnaireFileByUuid uuid = do
  tenantUuid <- asks currentTenantUuid
  createDeleteEntityByFn entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]
