module Wizard.Database.DAO.Questionnaire.QuestionnaireDAO where

import Control.Lens ((&), (.~), (^.))
import Control.Monad.Reader (asks)
import Data.Foldable (traverse_)
import qualified Data.List as L
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
import Wizard.Database.Mapping.Questionnaire.QuestionnaireDetail ()
import Wizard.Database.Mapping.Questionnaire.QuestionnaireEvent ()
import Wizard.Database.Mapping.Questionnaire.QuestionnaireSimple ()
import Wizard.Database.Mapping.Questionnaire.QuestionnaireSquash ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.AppContextHelpers
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireDetail
import Wizard.Model.Questionnaire.QuestionnaireEvent
import Wizard.Model.Questionnaire.QuestionnaireSimple
import Wizard.Model.Questionnaire.QuestionnaireSquash
import Wizard.Model.User.User
import Wizard.Util.Logger

entityName = "questionnaire"

pageLabel = "questionnaires"

findQuestionnaires :: AppContextM [Questionnaire]
findQuestionnaires = do
  appUuid <- asks _appContextAppUuid
  currentUser <- getCurrentUser
  if currentUser ^. role == _USER_ROLE_ADMIN
    then createFindEntitiesByFn entityName [appQueryUuid appUuid] >>= traverse enhance
    else do
      let sql = f' (qtnSelectSql (U.toString appUuid) (U.toString $ currentUser ^. uuid) "['VIEW']") [""]
      logInfoU _CMP_DATABASE sql
      let action conn = query_ conn (fromString sql)
      entities <- runDB action
      traverse enhance entities

findQuestionnairesForCurrentUserPage ::
     Maybe String -> Maybe Bool -> Maybe [String] -> Pageable -> [Sort] -> AppContextM (Page QuestionnaireDetail)
findQuestionnairesForCurrentUserPage mQuery mIsTemplate mUserUuids pageable sort
  -- 1. Prepare variables
 = do
  appUuid <- asks _appContextAppUuid
  let nameCondition = "qtn.name ~* ?"
  let isTemplateCondition =
        case mIsTemplate of
          Nothing -> ""
          Just True -> " AND qtn.is_template = true"
          Just False -> " AND qtn.is_template = false"
  let userUuidsJoin =
        case mUserUuids of
          Nothing -> ""
          Just [] -> ""
          Just _ -> "LEFT JOIN questionnaire_acl_user qtn_acl_user ON qtn.uuid = qtn_acl_user.questionnaire_uuid "
  let userUuidsCondition =
        case mUserUuids of
          Nothing -> ""
          Just userUuids ->
            let mapFn userUuid = " qtn_acl_user.user_uuid = '" ++ userUuid ++ "' "
             in " AND (" ++ L.intercalate " OR " (fmap mapFn userUuids) ++ ")"
  currentUser <- getCurrentUser
  let (sizeI, pageI, skip, limit) = preparePaginationVariables pageable
  -- 2. Get total count
  let sql =
        if currentUser ^. role == _USER_ROLE_ADMIN
          then f'
                 "SELECT COUNT(DISTINCT qtn.uuid) FROM questionnaire qtn %s WHERE %s %s %s %s"
                 [ userUuidsJoin
                 , f' "qtn.app_uuid = '%s' AND" [U.toString appUuid]
                 , nameCondition
                 , isTemplateCondition
                 , userUuidsCondition
                 ]
          else f'
                 "SELECT COUNT(DISTINCT qtn.uuid) \
                  \FROM questionnaire qtn \
                  \LEFT JOIN questionnaire_acl_user qtn_acl_user ON qtn.uuid = qtn_acl_user.questionnaire_uuid \
                  \LEFT JOIN questionnaire_acl_group qtn_acl_group ON qtn.uuid = qtn_acl_group.questionnaire_uuid \
                  \WHERE %s AND %s %s %s"
                 [ qtnWhereSql (U.toString appUuid) (U.toString $ currentUser ^. uuid) "['VIEW']"
                 , nameCondition
                 , isTemplateCondition
                 , userUuidsCondition
                 ]
  logInfoU _CMP_DATABASE sql
  let action conn = query conn (fromString sql) [regex mQuery]
  result <- runDB action
  let count =
        case result of
          [count] -> fromOnly count
          _ -> 0
  -- 3. Get entities
  let sqlBase =
        "SELECT DISTINCT qtn.uuid, \
                 \qtn.name, \
                 \qtn.description, \
                 \qtn.visibility, \
                 \qtn.sharing, \
                 \qtn.selected_tag_uuids::jsonb, \
                 \qtn.events::jsonb, \
                 \qtn.is_template, \
                 \qtn.created_at, \
                 \qtn.updated_at, \
                 \CASE \
                 \  WHEN qtn_mig.new_questionnaire_uuid IS NOT NULL THEN 'Migrating' \
                 \  WHEN qtn.package_id != (SELECT CONCAT(organization_id, ':', km_id, ':', \
                 \          (max(string_to_array(version, '.')::int[]))[1] || '.' || \
                 \          (max(string_to_array(version, '.')::int[]))[2] || '.' || \
                 \          (max(string_to_array(version, '.')::int[]))[3]) \
                 \      FROM package \
                 \      WHERE organization_id = pkg.organization_id \
                 \        AND km_id = pkg.km_id \
                 \      GROUP BY organization_id, km_id) THEN 'Outdated' \
                 \  WHEN qtn_mig.new_questionnaire_uuid IS NULL THEN 'Default' \
                 \  END, \
                 \pkg.id, \
                 \pkg.name, \
                 \pkg.version, \
                 \( \
                 \  SELECT array_agg(CONCAT(qtn_acl_user.uuid, '::', qtn_acl_user.perms, '::', u.uuid, '::', u.first_name, '::', u.last_name, '::', u.email, '::', u.image_url)) \
                 \  FROM questionnaire_acl_user qtn_acl_user \
                 \           JOIN user_entity u on u.uuid = qtn_acl_user.user_uuid \
                 \  WHERE questionnaire_uuid = qtn.uuid \
                 \  GROUP BY questionnaire_uuid \
                 \) as user_permissions \
                 \FROM questionnaire qtn \
                 \JOIN package pkg ON qtn.package_id = pkg.id \
                 \LEFT JOIN questionnaire_migration qtn_mig ON qtn.uuid = qtn_mig.new_questionnaire_uuid "
  let sql =
        if currentUser ^. role == _USER_ROLE_ADMIN
          then f'
                 "%s %s \
                 \WHERE %s %s %s %s %s \
                 \OFFSET %s LIMIT %s"
                 [ sqlBase
                 , userUuidsJoin
                 , f' "qtn.app_uuid = '%s' AND" [U.toString appUuid]
                 , nameCondition
                 , isTemplateCondition
                 , userUuidsCondition
                 , mapSortWithPrefix "qtn" sort
                 , show skip
                 , show sizeI
                 ]
          else f'
                 "%s \
                   \LEFT JOIN questionnaire_acl_user qtn_acl_user ON qtn.uuid = qtn_acl_user.questionnaire_uuid \
                   \LEFT JOIN questionnaire_acl_group qtn_acl_group ON qtn.uuid = qtn_acl_group.questionnaire_uuid \
                   \WHERE %s %s OFFSET %s LIMIT %s"
                 [ sqlBase
                 , qtnWhereSql (U.toString appUuid) (U.toString $ currentUser ^. uuid) "['VIEW']" ++
                   " AND " ++ nameCondition ++ isTemplateCondition ++ userUuidsCondition
                 , mapSortWithPrefix "qtn" sort
                 , show skip
                 , show sizeI
                 ]
  logInfoU _CMP_DATABASE sql
  let action conn = query conn (fromString sql) [regex mQuery]
  entities <- runDB action
  -- 5. Constructor response
  let metadata =
        PageMetadata
          { _pageMetadataSize = sizeI
          , _pageMetadataTotalElements = count
          , _pageMetadataTotalPages = computeTotalPage count sizeI
          , _pageMetadataNumber = pageI
          }
  return $ Page pageLabel metadata entities

findQuestionnairesByPackageId :: String -> AppContextM [Questionnaire]
findQuestionnairesByPackageId packageId = do
  appUuid <- asks _appContextAppUuid
  currentUser <- getCurrentUser
  if currentUser ^. role == _USER_ROLE_ADMIN
    then createFindEntitiesByFn entityName [appQueryUuid appUuid, ("package_id", packageId)] >>= traverse enhance
    else do
      let sql =
            f' (qtnSelectSql (U.toString appUuid) (U.toString $ currentUser ^. uuid) "['VIEW']") ["and package_id = ?"]
      logInfoU _CMP_DATABASE sql
      let action conn = query conn (fromString sql) [packageId]
      entities <- runDB action
      traverse enhance entities

findQuestionnairesByTemplateId :: String -> AppContextM [Questionnaire]
findQuestionnairesByTemplateId templateId = do
  appUuid <- asks _appContextAppUuid
  currentUser <- getCurrentUser
  if currentUser ^. role == _USER_ROLE_ADMIN
    then createFindEntitiesByFn entityName [appQueryUuid appUuid, ("template_id", templateId)] >>= traverse enhance
    else do
      let sql =
            f' (qtnSelectSql (U.toString appUuid) (U.toString $ currentUser ^. uuid) "['VIEW']") ["and template_id = ?"]
      logInfoU _CMP_DATABASE sql
      let action conn = query conn (fromString sql) [templateId]
      entities <- runDB action
      traverse enhance entities

findQuestionnairesOwnedByUser :: String -> AppContextM [Questionnaire]
findQuestionnairesOwnedByUser userUuid = do
  appUuid <- asks _appContextAppUuid
  currentUser <- getCurrentUser
  let sql = f' (qtnSelectSql (U.toString appUuid) (U.toString $ currentUser ^. uuid) "[]::text[]") [""]
  logInfoU _CMP_DATABASE sql
  let action conn = query_ conn (fromString sql)
  entities <- runDB action
  traverse enhance entities

findQuestionnaireWithZeroAcl :: AppContextM [Questionnaire]
findQuestionnaireWithZeroAcl = do
  let sql =
        f'
          "SELECT qtn.* \
               \FROM %s qtn \
               \LEFT JOIN questionnaire_acl_user qtn_acl_user ON qtn.uuid = qtn_acl_user.questionnaire_uuid \
               \LEFT JOIN questionnaire_acl_group qtn_acl_group ON qtn.uuid = qtn_acl_group.questionnaire_uuid \
               \WHERE qtn_acl_user.uuid IS NULL \
               \AND qtn_acl_group.uuid IS NULL \
               \AND qtn.updated_at < now() - INTERVAL '30 days'"
          [entityName]
  logInfoU _CMP_DATABASE sql
  let action conn = query_ conn (fromString sql)
  runDB action

findQuestionnaireById :: String -> AppContextM Questionnaire
findQuestionnaireById qtnUuid = do
  appUuid <- asks _appContextAppUuid
  entity <- createFindEntityByFn entityName [appQueryUuid appUuid, ("uuid", qtnUuid)]
  enhance entity

findQuestionnaireById' :: String -> AppContextM (Maybe Questionnaire)
findQuestionnaireById' qtnUuid = do
  appUuid <- asks _appContextAppUuid
  mEntity <- createFindEntityByFn' entityName [appQueryUuid appUuid, ("uuid", qtnUuid)]
  case mEntity of
    Just entity -> enhance entity >>= return . Just
    Nothing -> return Nothing

findQuestionnaireSimpleById :: String -> AppContextM QuestionnaireSimple
findQuestionnaireSimpleById uuid = do
  appUuid <- asks _appContextAppUuid
  createFindEntityWithFieldsByFn "uuid, name" entityName [appQueryUuid appUuid, ("uuid", uuid)]

findQuestionnaireSimpleById' :: String -> AppContextM (Maybe QuestionnaireSimple)
findQuestionnaireSimpleById' uuid = do
  appUuid <- asks _appContextAppUuid
  createFindEntityWithFieldsByFn' "uuid, name" entityName [appQueryUuid appUuid, ("uuid", uuid)]

findQuestionnaireEventsById :: String -> AppContextM [QuestionnaireEvent]
findQuestionnaireEventsById uuid = do
  appUuid <- asks _appContextAppUuid
  let sql = "SELECT events FROM questionnaire qtn WHERE app_uuid = ? AND uuid = ?"
  logInfoU _CMP_DATABASE sql
  let action conn = query conn (fromString sql) [toField appUuid, toField uuid]
  entities <- runDB action
  case entities of
    [entity] -> return . _questionnaireEventBundleEvents $ entity
    _ -> return []

findQuestionnaireForSquashing :: AppContextM [U.UUID]
findQuestionnaireForSquashing = do
  let sql = "SELECT uuid FROM questionnaire qtn WHERE squashed = false"
  logInfoU _CMP_DATABASE sql
  let action conn = query_ conn (fromString sql)
  entities <- runDB action
  return . concat $ entities

findQuestionnaireSquashById :: String -> AppContextM QuestionnaireSquash
findQuestionnaireSquashById uuid = createFindEntityWithFieldsByFn "uuid, events, versions" entityName [("uuid", uuid)]

countQuestionnaires :: AppContextM Int
countQuestionnaires = createCountFn entityName

insertQuestionnaire :: Questionnaire -> AppContextM Int64
insertQuestionnaire qtn = do
  createInsertFn entityName qtn
  traverse_ insertQuestionnairePermRecord (qtn ^. permissions)
  return 1

updateQuestionnaireById :: Questionnaire -> AppContextM ()
updateQuestionnaireById qtn = do
  appUuid <- asks _appContextAppUuid
  let params = toRow qtn ++ [toField appUuid, toField . U.toText $ qtn ^. uuid]
  let sql =
        "UPDATE questionnaire SET uuid = ?, name = ?, visibility = ?, sharing = ?, package_id = ?, selected_tag_uuids = ?, template_id = ?, format_uuid = ?, creator_uuid = ?, events = ?, versions = ?, created_at = ?, updated_at = ?, description = ?, is_template = ?, squashed = ?, app_uuid = ? WHERE  app_uuid = ? AND uuid = ?"
  logInfoU _CMP_DATABASE sql
  let action conn = execute conn (fromString sql) params
  runDB action
  deleteQuestionnairePermRecordsFiltered [("questionnaire_uuid", U.toString $ qtn ^. uuid)]
  traverse_ insertQuestionnairePermRecord (qtn ^. permissions)

updateQuestionnaireEventsByUuid :: String -> [QuestionnaireEvent] -> AppContextM ()
updateQuestionnaireEventsByUuid qtnUuid events = do
  appUuid <- asks _appContextAppUuid
  let sql = "UPDATE questionnaire SET squashed = true, events = ? WHERE app_uuid = ? AND uuid = ?"
  logInfoU _CMP_DATABASE sql
  let action conn = execute conn (fromString sql) [toJSONField events, toField appUuid, toField qtnUuid]
  runDB action
  return ()

appendQuestionnaireEventByUuid :: String -> [QuestionnaireEvent] -> AppContextM ()
appendQuestionnaireEventByUuid qtnUuid events = do
  appUuid <- asks _appContextAppUuid
  let sql =
        "UPDATE questionnaire SET squashed = false, events = events::jsonb || ?::jsonb WHERE app_uuid = ? AND uuid = ?"
  logInfoU _CMP_DATABASE sql
  let action conn = execute conn (fromString sql) [toJSONField events, toField appUuid, toField qtnUuid]
  runDB action
  return ()

deleteQuestionnaires :: AppContextM Int64
deleteQuestionnaires = createDeleteEntitiesFn entityName

deleteQuestionnairesFiltered :: [(String, String)] -> AppContextM Int64
deleteQuestionnairesFiltered params = do
  appUuid <- asks _appContextAppUuid
  createDeleteEntitiesByFn entityName (appQueryUuid appUuid : params)

deleteQuestionnaireById :: String -> AppContextM Int64
deleteQuestionnaireById uuid = do
  appUuid <- asks _appContextAppUuid
  createDeleteEntityByFn entityName [appQueryUuid appUuid, ("uuid", uuid)]

-- ------------------------------------------------------------------------------------------------------------------------------
-- PRIVATE
-- ------------------------------------------------------------------------------------------------------------------------------
qtnSelectSql appUuid userUuid perm =
  f'
    "SELECT qtn.* \
    \FROM questionnaire qtn \
    \LEFT JOIN questionnaire_acl_user qtn_acl_user ON qtn.uuid = qtn_acl_user.questionnaire_uuid \
    \LEFT JOIN questionnaire_acl_group qtn_acl_group ON qtn.uuid = qtn_acl_group.questionnaire_uuid \
    \WHERE %s %s"
    [qtnWhereSql appUuid userUuid perm]

qtnWhereSql appUuid userUuid perm =
  f'
    "qtn.app_uuid = '%s' \
    \AND (visibility = 'VisibleEditQuestionnaire' \
    \OR visibility = 'VisibleCommentQuestionnaire' \
    \OR visibility = 'VisibleViewQuestionnaire' \
    \OR (visibility = 'PrivateQuestionnaire' and qtn_acl_user.user_uuid = '%s' AND qtn_acl_user.perms @> ARRAY %s))"
    [appUuid, userUuid, perm]

enhance qtn = do
  ps <- findQuestionnairePermRecordsFiltered [("questionnaire_uuid", U.toString $ qtn ^. uuid)]
  return $ qtn & permissions .~ ps
