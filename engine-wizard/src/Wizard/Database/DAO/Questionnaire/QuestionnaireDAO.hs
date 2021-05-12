module Wizard.Database.DAO.Questionnaire.QuestionnaireDAO where

import Control.Lens ((&), (.~), (^.))
import Data.Foldable (traverse_)
import Data.String (fromString)
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import LensesConfig
import Shared.Model.Common.Page
import Shared.Model.Common.PageMetadata
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Questionnaire.QuestionnaireAclDAO
  ( deleteQuestionnairePermRecordsFiltered
  , findQuestionnairePermRecordsFiltered
  , insertQuestionnairePermRecord
  )
import Wizard.Database.Mapping.Questionnaire.Questionnaire ()
import Wizard.Database.Mapping.Questionnaire.QuestionnaireEvent ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.AppContextHelpers
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireEvent
import Wizard.Model.User.User
import Wizard.Util.Logger

entityName = "questionnaire"

pageLabel = "questionnaires"

findQuestionnaires :: AppContextM [Questionnaire]
findQuestionnaires = do
  currentUser <- getCurrentUser
  if currentUser ^. role == _USER_ROLE_ADMIN
    then createFindEntitiesFn entityName >>= traverse enhance
    else do
      let sql = f' (qtnSelectSql (U.toString $ currentUser ^. uuid) "['VIEW']") [""]
      logInfo _CMP_DATABASE sql
      let action conn = query_ conn (fromString sql)
      entities <- runDB action
      traverse enhance entities

findQuestionnairesForCurrentUserPage :: Maybe String -> Pageable -> [Sort] -> AppContextM (Page Questionnaire)
findQuestionnairesForCurrentUserPage mQuery pageable sort
  -- 1. Prepare variables
 = do
  let condition = "name ~* ?"
  currentUser <- getCurrentUser
  let (sizeI, pageI, skip, limit) = preparePaginationVariables pageable
  -- 2. Get total count
  let sql =
        if currentUser ^. role == _USER_ROLE_ADMIN
          then f' "SELECT COUNT(*) FROM questionnaire WHERE %s" [condition]
          else f'
                 "SELECT COUNT(*) \
                  \FROM questionnaire qtn \
                  \LEFT JOIN questionnaire_acl_user qtn_acl_user ON qtn.uuid = qtn_acl_user.questionnaire_uuid \
                  \LEFT JOIN questionnaire_acl_group qtn_acl_group ON qtn.uuid = qtn_acl_group.questionnaire_uuid \
                  \WHERE %s AND %s"
                 [qtnWhereSql (U.toString $ currentUser ^. uuid) "['VIEW']", condition]
  logInfo _CMP_DATABASE sql
  let action conn = query conn (fromString sql) [regex mQuery]
  result <- runDB action
  let count =
        case result of
          [count] -> fromOnly count
          _ -> 0
  -- 3. Get entities
  let sql =
        if currentUser ^. role == _USER_ROLE_ADMIN
          then f'
                 "SELECT * FROM questionnaire WHERE %s %s OFFSET %s LIMIT %s"
                 [condition, mapSort sort, show skip, show sizeI]
          else f'
                 (qtnSelectSql (U.toString $ currentUser ^. uuid) "['VIEW']" ++ " %s OFFSET %s LIMIT %s")
                 [" AND " ++ condition, mapSort sort, show skip, show sizeI]
  logInfo _CMP_DATABASE sql
  let action conn = query conn (fromString sql) [regex mQuery]
  entities <- runDB action
  -- 4. Load additional data
  enhancedEntities <- traverse enhance entities
  -- 5. Constructor response
  let metadata =
        PageMetadata
          { _pageMetadataSize = sizeI
          , _pageMetadataTotalElements = count
          , _pageMetadataTotalPages = computeTotalPage count sizeI
          , _pageMetadataNumber = pageI
          }
  return $ Page pageLabel metadata enhancedEntities

findQuestionnairesByPackageId :: String -> AppContextM [Questionnaire]
findQuestionnairesByPackageId packageId = do
  currentUser <- getCurrentUser
  if currentUser ^. role == _USER_ROLE_ADMIN
    then createFindEntitiesByFn entityName [("package_id", packageId)] >>= traverse enhance
    else do
      let sql = f' (qtnSelectSql (U.toString $ currentUser ^. uuid) "['VIEW']") ["and package_id = ?"]
      logInfo _CMP_DATABASE sql
      let action conn = query conn (fromString sql) [packageId]
      entities <- runDB action
      traverse enhance entities

findQuestionnairesByTemplateId :: String -> AppContextM [Questionnaire]
findQuestionnairesByTemplateId templateId = do
  currentUser <- getCurrentUser
  if currentUser ^. role == _USER_ROLE_ADMIN
    then createFindEntitiesByFn entityName [("template_id", templateId)] >>= traverse enhance
    else do
      let sql = f' (qtnSelectSql (U.toString $ currentUser ^. uuid) "['VIEW']") ["and template_id = ?"]
      logInfo _CMP_DATABASE sql
      let action conn = query conn (fromString sql) [templateId]
      entities <- runDB action
      traverse enhance entities

findQuestionnairesOwnedByUser :: String -> AppContextM [Questionnaire]
findQuestionnairesOwnedByUser userUuid = do
  currentUser <- getCurrentUser
  let sql = f' (qtnSelectSql (U.toString $ currentUser ^. uuid) "[]::text[]") [""]
  logInfo _CMP_DATABASE sql
  let action conn = query_ conn (fromString sql)
  entities <- runDB action
  traverse enhance entities

findQuestionnaireById :: String -> AppContextM Questionnaire
findQuestionnaireById qtnUuid = do
  entity <- createFindEntityByFn entityName "uuid" qtnUuid
  enhance entity

findQuestionnaireById' :: String -> AppContextM (Maybe Questionnaire)
findQuestionnaireById' qtnUuid = do
  mEntity <- createFindEntityByFn' entityName "uuid" qtnUuid
  case mEntity of
    Just entity -> enhance entity >>= return . Just
    Nothing -> return Nothing

findQuestionnaireEventsById :: String -> AppContextM [QuestionnaireEvent]
findQuestionnaireEventsById uuid = do
  let sql = "SELECT events FROM questionnaire WHERE uuid = ?"
  logInfo _CMP_DATABASE sql
  let action conn = query conn (fromString sql) [toField uuid]
  entities <- runDB action
  case entities of
    [entity] -> return . _questionnaireEventBundleEvents $ entity
    _ -> return []

countQuestionnaires :: AppContextM Int
countQuestionnaires = createCountFn entityName

insertQuestionnaire :: Questionnaire -> AppContextM Int64
insertQuestionnaire qtn = do
  createInsertFn entityName qtn
  traverse_ insertQuestionnairePermRecord (qtn ^. permissions)
  return 1

updateQuestionnaireById :: Questionnaire -> AppContextM ()
updateQuestionnaireById qtn = do
  let params = toRow qtn ++ [toField . U.toText $ qtn ^. uuid]
  let action conn =
        execute
          conn
          "UPDATE questionnaire SET uuid = ?, name = ?, visibility = ?, sharing = ?, package_id = ?, selected_tag_uuids = ?, template_id = ?, format_uuid = ?, creator_uuid = ?, events = ?, versions = ?, created_at = ?, updated_at = ? WHERE uuid = ?"
          params
  runDB action
  deleteQuestionnairePermRecordsFiltered [("questionnaire_uuid", U.toString $ qtn ^. uuid)]
  traverse_ insertQuestionnairePermRecord (qtn ^. permissions)

updateQuestionnaireEventsById :: String -> [QuestionnaireEvent] -> AppContextM ()
updateQuestionnaireEventsById qtnUuid events = do
  let sql = "UPDATE questionnaire SET events = ? WHERE uuid = ?"
  logInfo _CMP_DATABASE sql
  let action conn = execute conn (fromString sql) [toJSONField events, toField qtnUuid]
  runDB action
  return ()

deleteQuestionnaires :: AppContextM Int64
deleteQuestionnaires = createDeleteEntitiesFn entityName

deleteQuestionnairesFiltered :: [(String, String)] -> AppContextM Int64
deleteQuestionnairesFiltered = createDeleteEntitiesByFn entityName

deleteQuestionnaireById :: String -> AppContextM Int64
deleteQuestionnaireById = createDeleteEntityByFn entityName "uuid"

-- ------------------------------------------------------------------------------------------------------------------------------
-- PRIVATE
-- ------------------------------------------------------------------------------------------------------------------------------
qtnSelectSql userUuid perm =
  f'
    "SELECT qtn.* \
    \FROM questionnaire qtn \
    \LEFT JOIN questionnaire_acl_user qtn_acl_user ON qtn.uuid = qtn_acl_user.questionnaire_uuid \
    \LEFT JOIN questionnaire_acl_group qtn_acl_group ON qtn.uuid = qtn_acl_group.questionnaire_uuid \
    \WHERE %s %s"
    [qtnWhereSql userUuid perm]

qtnWhereSql userUuid perm =
  f'
    "(visibility = 'VisibleEditQuestionnaire' \
    \OR visibility = 'VisibleViewQuestionnaire' \
    \OR (visibility = 'PrivateQuestionnaire' and qtn_acl_user.user_uuid = '%s' AND qtn_acl_user.perms @> ARRAY %s))"
    [userUuid, perm]

enhance qtn = do
  ps <- findQuestionnairePermRecordsFiltered [("questionnaire_uuid", U.toString $ qtn ^. uuid)]
  return $ qtn & permissions .~ ps
