module Wizard.Database.DAO.Questionnaire.QuestionnaireDAO where

import Control.Monad.Reader (asks)
import Data.Foldable (traverse_)
import qualified Data.List as L
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Model.Common.Page
import Shared.Model.Common.PageMetadata
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Shared.Util.String (replace, trim)
import Wizard.Api.Resource.User.UserDTO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Questionnaire.QuestionnaireAclDAO (
  deleteQuestionnairePermRecordsFiltered,
  findQuestionnairePermRecordsFiltered,
  insertQuestionnairePermRecord,
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
import Wizard.Model.Report.Report
import Wizard.Model.User.User
import Wizard.Util.Logger

entityName = "questionnaire"

pageLabel = "questionnaires"

findQuestionnaires :: AppContextM [Questionnaire]
findQuestionnaires = do
  appUuid <- asks currentAppUuid
  currentUser <- getCurrentUser
  if currentUser.uRole == _USER_ROLE_ADMIN
    then createFindEntitiesByFn entityName [appQueryUuid appUuid] >>= traverse enhance
    else do
      let sql = f' (qtnSelectSql (U.toString appUuid) (U.toString $ currentUser.uuid) "['VIEW']") [""]
      logInfoU _CMP_DATABASE sql
      let action conn = query_ conn (fromString sql)
      entities <- runDB action
      traverse enhance entities

findQuestionnairesForCurrentUserPage
  :: Maybe String
  -> Maybe Bool
  -> Maybe Bool
  -> Maybe [String]
  -> Maybe String
  -> Maybe [String]
  -> Maybe String
  -> Maybe [String]
  -> Maybe String
  -> Pageable
  -> [Sort]
  -> AppContextM (Page QuestionnaireDetail)
findQuestionnairesForCurrentUserPage mQuery mIsTemplate mIsMigrating mProjectTags mProjectTagsOp mUserUuids mUserUuidsOp mPackageIds mPackageIdsOp pageable sort =
  -- 1. Prepare variables
  do
    appUuid <- asks currentAppUuid
    let nameCondition = "qtn.name ~* ?"
    let isTemplateCondition =
          case mIsTemplate of
            Nothing -> ""
            Just True -> " AND qtn.is_template = true"
            Just False -> " AND qtn.is_template = false"
    let isMigratingCondition =
          case mIsMigrating of
            Nothing -> ""
            Just True -> " AND qtn_mig.new_questionnaire_uuid IS NOT NULL"
            Just False -> " AND qtn_mig.new_questionnaire_uuid IS NULL"
    let projectTagsCondition =
          case mProjectTags of
            Nothing -> ""
            Just [] -> ""
            Just projectTags ->
              let mapFn _ = " qtn.project_tags @> ARRAY [?]"
               in if isAndOperator mProjectTagsOp
                    then " AND (" ++ L.intercalate " AND " (fmap mapFn projectTags) ++ ")"
                    else " AND (" ++ L.intercalate " OR " (fmap mapFn projectTags) ++ ")"
    let userUuidsJoin =
          case mUserUuids of
            Nothing -> ""
            Just [] -> ""
            Just _ -> "LEFT JOIN questionnaire_acl_user qtn_acl_user ON qtn.uuid = qtn_acl_user.questionnaire_uuid "
    let qtnMigrationJoin =
          case mIsMigrating of
            Nothing -> ""
            Just _ -> "LEFT JOIN questionnaire_migration qtn_mig ON qtn.uuid = qtn_mig.new_questionnaire_uuid "
    let userUuidsCondition =
          case mUserUuids of
            Nothing -> ""
            Just [] -> ""
            Just userUuids ->
              if isAndOperator mUserUuidsOp
                then
                  f'
                    " AND %s = ( \
                    \SELECT COUNT(DISTINCT user_uuid) \
                    \FROM questionnaire_acl_user \
                    \WHERE questionnaire_uuid = qtn.uuid AND user_uuid in (%s)) "
                    [show . length $ userUuids, generateQuestionMarks userUuids]
                else
                  let mapFn _ = " qtn_acl_user.user_uuid = ? "
                   in " AND (" ++ L.intercalate " OR " (fmap mapFn userUuids) ++ ")"
    let mPackageIdsLike = fmap (fmap (replace "all" "%")) mPackageIds
    let packageCondition =
          case mPackageIds of
            Nothing -> ""
            Just [] -> ""
            Just packageIds ->
              let mapFn _ = " qtn.package_id LIKE ?"
               in if isAndOperator mPackageIdsOp
                    then " AND (" ++ L.intercalate " AND " (fmap mapFn packageIds) ++ ")"
                    else " AND (" ++ L.intercalate " OR " (fmap mapFn packageIds) ++ ")"
    currentUser <- getCurrentUser
    let (sizeI, pageI, skip, limit) = preparePaginationVariables pageable
    -- 2. Get total count
    let sql =
          fromString $
            if currentUser.uRole == _USER_ROLE_ADMIN
              then
                f'
                  "SELECT COUNT(DISTINCT qtn.uuid) FROM questionnaire qtn %s %s WHERE %s %s %s %s %s %s %s"
                  [ userUuidsJoin
                  , qtnMigrationJoin
                  , f' "qtn.app_uuid = '%s' AND" [U.toString appUuid]
                  , nameCondition
                  , isTemplateCondition
                  , isMigratingCondition
                  , projectTagsCondition
                  , userUuidsCondition
                  , packageCondition
                  ]
              else
                f'
                  "SELECT COUNT(DISTINCT qtn.uuid) \
                  \FROM questionnaire qtn \
                  \LEFT JOIN questionnaire_acl_user qtn_acl_user ON qtn.uuid = qtn_acl_user.questionnaire_uuid \
                  \LEFT JOIN questionnaire_acl_group qtn_acl_group ON qtn.uuid = qtn_acl_group.questionnaire_uuid \
                  \%s \
                  \WHERE %s AND %s %s %s %s %s %s"
                  [ qtnMigrationJoin
                  , qtnWhereSql (U.toString appUuid) (U.toString $ currentUser.uuid) "['VIEW']"
                  , nameCondition
                  , isTemplateCondition
                  , isMigratingCondition
                  , projectTagsCondition
                  , userUuidsCondition
                  , packageCondition
                  ]
    let params = [regex mQuery] ++ fromMaybe [] mProjectTags ++ fromMaybe [] mUserUuids ++ fromMaybe [] mPackageIdsLike
    logQuery sql params
    let action conn = query conn sql params
    result <- runDB action
    let count =
          case result of
            [count] -> fromOnly count
            _ -> 0
    -- 3. Get entities
    let sqlBase =
          f'
            "SELECT DISTINCT qtn.uuid, \
            \qtn.name, \
            \qtn.description, \
            \qtn.visibility, \
            \qtn.sharing, \
            \qtn.selected_question_tag_uuids::jsonb, \
            \qtn.events::jsonb, \
            \qtn.is_template, \
            \qtn.answered_questions, \
            \qtn.unanswered_questions, \
            \qtn.created_at, \
            \qtn.updated_at, \
            \CASE \
            \  WHEN qtn_mig.new_questionnaire_uuid IS NOT NULL THEN 'Migrating' \
            \  WHEN qtn.package_id != get_newest_package(pkg.organization_id, pkg.km_id, '%s') THEN 'Outdated' \
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
            \JOIN package pkg ON qtn.package_id = pkg.id AND qtn.app_uuid = pkg.app_uuid \
            \LEFT JOIN questionnaire_migration qtn_mig ON qtn.uuid = qtn_mig.new_questionnaire_uuid "
            [U.toString appUuid]
    let sql =
          fromString $
            if currentUser.uRole == _USER_ROLE_ADMIN
              then
                f'
                  "%s %s \
                  \WHERE %s %s %s %s %s %s %s  %s \
                  \OFFSET %s LIMIT %s"
                  [ sqlBase
                  , userUuidsJoin
                  , f' "qtn.app_uuid = '%s' AND" [U.toString appUuid]
                  , nameCondition
                  , isTemplateCondition
                  , isMigratingCondition
                  , projectTagsCondition
                  , userUuidsCondition
                  , packageCondition
                  , mapSortWithPrefix "qtn" sort
                  , show skip
                  , show sizeI
                  ]
              else
                f'
                  "%s \
                  \LEFT JOIN questionnaire_acl_user qtn_acl_user ON qtn.uuid = qtn_acl_user.questionnaire_uuid \
                  \LEFT JOIN questionnaire_acl_group qtn_acl_group ON qtn.uuid = qtn_acl_group.questionnaire_uuid \
                  \WHERE %s %s OFFSET %s LIMIT %s"
                  [ sqlBase
                  , qtnWhereSql (U.toString appUuid) (U.toString $ currentUser.uuid) "['VIEW']"
                      ++ " AND "
                      ++ nameCondition
                      ++ isTemplateCondition
                      ++ isMigratingCondition
                      ++ projectTagsCondition
                      ++ userUuidsCondition
                      ++ packageCondition
                  , mapSortWithPrefix "qtn" sort
                  , show skip
                  , show sizeI
                  ]
    logQuery sql params
    let action conn = query conn sql params
    entities <- runDB action
    -- 5. Constructor response
    let metadata =
          PageMetadata
            { size = sizeI
            , totalElements = count
            , totalPages = computeTotalPage count sizeI
            , number = pageI
            }
    return $ Page pageLabel metadata entities

findQuestionnairesByPackageId :: String -> AppContextM [Questionnaire]
findQuestionnairesByPackageId packageId = do
  appUuid <- asks currentAppUuid
  currentUser <- getCurrentUser
  if currentUser.uRole == _USER_ROLE_ADMIN
    then createFindEntitiesByFn entityName [appQueryUuid appUuid, ("package_id", packageId)] >>= traverse enhance
    else do
      let sql =
            fromString $
              f' (qtnSelectSql (U.toString appUuid) (U.toString $ currentUser.uuid) "['VIEW']") ["and package_id = ?"]
      let params = [packageId]
      logQuery sql params
      let action conn = query conn sql params
      entities <- runDB action
      traverse enhance entities

findQuestionnairesByTemplateId :: String -> AppContextM [Questionnaire]
findQuestionnairesByTemplateId templateId = do
  appUuid <- asks currentAppUuid
  currentUser <- getCurrentUser
  if currentUser.uRole == _USER_ROLE_ADMIN
    then createFindEntitiesByFn entityName [appQueryUuid appUuid, ("template_id", templateId)] >>= traverse enhance
    else do
      let sql =
            fromString $
              f' (qtnSelectSql (U.toString appUuid) (U.toString $ currentUser.uuid) "['VIEW']") ["and template_id = ?"]
      let params = [templateId]
      logQuery sql params
      let action conn = query conn sql params
      entities <- runDB action
      traverse enhance entities

findQuestionnairesOwnedByUser :: String -> AppContextM [Questionnaire]
findQuestionnairesOwnedByUser userUuid = do
  appUuid <- asks currentAppUuid
  currentUser <- getCurrentUser
  let sql = f' (qtnSelectSql (U.toString appUuid) (U.toString $ currentUser.uuid) "[]::text[]") [""]
  logInfoU _CMP_DATABASE (trim sql)
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
  logInfoU _CMP_DATABASE (trim sql)
  let action conn = query_ conn (fromString sql)
  runDB action

findQuestionnaireUuids :: AppContextM [U.UUID]
findQuestionnaireUuids = do
  appUuid <- asks currentAppUuid
  let sql = fromString $ f' "SELECT %s FROM %s WHERE app_uuid = ?" ["uuid", entityName]
  let params = [toField appUuid]
  logQuery sql params
  let action conn = query conn sql params
  entities <- runDB action
  return . concat $ entities

findQuestionnaireUuids' :: AppContextM [U.UUID]
findQuestionnaireUuids' = do
  let sql = f' "SELECT %s FROM %s" ["uuid", entityName]
  logInfoU _CMP_DATABASE (trim sql)
  let action conn = query_ conn (fromString sql)
  entities <- runDB action
  return . concat $ entities

findQuestionnaireById :: String -> AppContextM Questionnaire
findQuestionnaireById qtnUuid = do
  appUuid <- asks currentAppUuid
  entity <- createFindEntityByFn entityName [appQueryUuid appUuid, ("uuid", qtnUuid)]
  enhance entity

findQuestionnaireById' :: String -> AppContextM (Maybe Questionnaire)
findQuestionnaireById' qtnUuid = do
  appUuid <- asks currentAppUuid
  mEntity <- createFindEntityByFn' entityName [appQueryUuid appUuid, ("uuid", qtnUuid)]
  case mEntity of
    Just entity -> enhance entity >>= return . Just
    Nothing -> return Nothing

findQuestionnaireSimpleById :: String -> AppContextM QuestionnaireSimple
findQuestionnaireSimpleById uuid = do
  appUuid <- asks currentAppUuid
  createFindEntityWithFieldsByFn "uuid, name" False entityName [appQueryUuid appUuid, ("uuid", uuid)]

findQuestionnaireSimpleById' :: String -> AppContextM (Maybe QuestionnaireSimple)
findQuestionnaireSimpleById' uuid = do
  appUuid <- asks currentAppUuid
  createFindEntityWithFieldsByFn' "uuid, name" entityName [appQueryUuid appUuid, ("uuid", uuid)]

findQuestionnaireEventsById :: String -> AppContextM [QuestionnaireEvent]
findQuestionnaireEventsById uuid = do
  appUuid <- asks currentAppUuid
  let sql = fromString "SELECT events FROM questionnaire qtn WHERE app_uuid = ? AND uuid = ?"
  let params = [toField appUuid, toField uuid]
  logQuery sql params
  let action conn = query conn sql params
  entities <- runDB action :: AppContextM [QuestionnaireEventBundle]
  case entities of
    [entity] -> return entity.events
    _ -> return []

findQuestionnaireForSquashing :: AppContextM [U.UUID]
findQuestionnaireForSquashing = do
  let sql = "SELECT uuid FROM questionnaire qtn WHERE squashed = false"
  logInfoU _CMP_DATABASE (trim sql)
  let action conn = query_ conn (fromString sql)
  entities <- runDB action
  return . concat $ entities

findQuestionnaireSquashById :: String -> AppContextM QuestionnaireSquash
findQuestionnaireSquashById uuid =
  createFindEntityWithFieldsByFn "uuid, events, versions" False entityName [("uuid", uuid)]

countQuestionnaires :: AppContextM Int
countQuestionnaires = do
  appUuid <- asks currentAppUuid
  countQuestionnairesWithApp (U.toString appUuid)

countQuestionnairesWithApp :: String -> AppContextM Int
countQuestionnairesWithApp appUuid = createCountByFn entityName appCondition [appUuid]

insertQuestionnaire :: Questionnaire -> AppContextM Int64
insertQuestionnaire qtn = do
  createInsertFn entityName qtn
  traverse_ insertQuestionnairePermRecord qtn.permissions
  return 1

updateQuestionnaireById :: Questionnaire -> AppContextM ()
updateQuestionnaireById qtn = do
  appUuid <- asks currentAppUuid
  let sql =
        fromString
          "UPDATE questionnaire SET uuid = ?, name = ?, visibility = ?, sharing = ?, package_id = ?, selected_question_tag_uuids = ?, template_id = ?, format_uuid = ?, created_by = ?, events = ?, versions = ?, created_at = ?, updated_at = ?, description = ?, is_template = ?, squashed = ?, app_uuid = ?, project_tags = ?, answered_questions = ?, unanswered_questions = ? WHERE  app_uuid = ? AND uuid = ?"
  let params = toRow qtn ++ [toField appUuid, toField . U.toText $ qtn.uuid]
  logInsertAndUpdate sql params
  let action conn = execute conn sql params
  runDB action
  deleteQuestionnairePermRecordsFiltered [("questionnaire_uuid", U.toString qtn.uuid)]
  traverse_ insertQuestionnairePermRecord qtn.permissions

updateQuestionnaireEventsByUuid :: String -> Bool -> [QuestionnaireEvent] -> AppContextM ()
updateQuestionnaireEventsByUuid qtnUuid squashed events = do
  let sql = fromString "UPDATE questionnaire SET squashed = ?, events = ?, updated_at = now() WHERE uuid = ?"
  let params = [toField squashed, toJSONField events, toField qtnUuid]
  logInsertAndUpdate sql params
  let action conn = execute conn sql params
  runDB action
  return ()

updateQuestionnaireEventsByUuid' :: String -> Bool -> [QuestionnaireEvent] -> AppContextM ()
updateQuestionnaireEventsByUuid' qtnUuid squashed events = do
  let sql = fromString "UPDATE questionnaire SET squashed = ?, events = ? WHERE uuid = ?"
  let params = [toField squashed, toJSONField events, toField qtnUuid]
  logInsertAndUpdate sql params
  let action conn = execute conn sql params
  runDB action
  return ()

updateQuestionnaireEventsByUuid'' :: String -> [QuestionnaireEvent] -> AppContextM ()
updateQuestionnaireEventsByUuid'' qtnUuid events = do
  let sql = fromString "UPDATE questionnaire SET events = ? WHERE uuid = ?"
  let params = [toJSONField events, toField qtnUuid]
  logInsertAndUpdate sql params
  let action conn = execute conn sql params
  runDB action
  return ()

updateQuestionnaireIndicationByUuid :: String -> PhasesAnsweredIndication -> AppContextM ()
updateQuestionnaireIndicationByUuid qtnUuid phasesAnsweredIndication = do
  let sql = fromString "UPDATE questionnaire SET answered_questions = ?, unanswered_questions = ? WHERE uuid = ?"
  let params =
        [ toField $ phasesAnsweredIndication.answeredQuestions
        , toField $ phasesAnsweredIndication.unansweredQuestions
        , toField qtnUuid
        ]
  logInsertAndUpdate sql params
  let action conn = execute conn sql params
  runDB action
  return ()

updateQuestionnaireEventsWithIndicationByUuid
  :: String -> Bool -> [QuestionnaireEvent] -> PhasesAnsweredIndication -> AppContextM ()
updateQuestionnaireEventsWithIndicationByUuid qtnUuid squashed events phasesAnsweredIndication = do
  let sql =
        fromString
          "UPDATE questionnaire SET squashed = ?, events = ?, answered_questions = ?, unanswered_questions = ?, updated_at = now() WHERE uuid = ?"
  let params =
        [ toField squashed
        , toJSONField events
        , toField $ phasesAnsweredIndication.answeredQuestions
        , toField $ phasesAnsweredIndication.unansweredQuestions
        , toField qtnUuid
        ]
  logInsertAndUpdate sql params
  let action conn = execute conn sql params
  runDB action
  return ()

appendQuestionnaireEventByUuid :: String -> [QuestionnaireEvent] -> PhasesAnsweredIndication -> AppContextM ()
appendQuestionnaireEventByUuid qtnUuid events phasesAnsweredIndication = do
  appUuid <- asks currentAppUuid
  let sql =
        fromString
          "UPDATE questionnaire SET squashed = false, events = events::jsonb || ?::jsonb, answered_questions = ?, unanswered_questions = ?, updated_at = now() WHERE app_uuid = ? AND uuid = ?"
  let params =
        [ toJSONField events
        , toField $ phasesAnsweredIndication.answeredQuestions
        , toField $ phasesAnsweredIndication.unansweredQuestions
        , toField appUuid
        , toField qtnUuid
        ]
  logInsertAndUpdate sql params
  let action conn = execute conn sql params
  runDB action
  return ()

appendQuestionnaireEventByUuid' :: String -> [QuestionnaireEvent] -> AppContextM ()
appendQuestionnaireEventByUuid' qtnUuid events = do
  appUuid <- asks currentAppUuid
  let sql =
        fromString
          "UPDATE questionnaire SET squashed = false, events = events::jsonb || ?::jsonb, updated_at = now() WHERE app_uuid = ? AND uuid = ?"
  let params = [toJSONField events, toField appUuid, toField qtnUuid]
  logInsertAndUpdate sql params
  let action conn = execute conn sql params
  runDB action
  return ()

clearQuestionnaireCreatedBy :: U.UUID -> AppContextM ()
clearQuestionnaireCreatedBy userUuid = do
  let sql = fromString "UPDATE questionnaire SET created_by = null WHERE created_by = ?"
  let params = [toField userUuid]
  logInsertAndUpdate sql params
  let action conn = execute conn sql params
  runDB action
  return ()

deleteQuestionnaires :: AppContextM Int64
deleteQuestionnaires = createDeleteEntitiesFn entityName

deleteQuestionnairesFiltered :: [(String, String)] -> AppContextM Int64
deleteQuestionnairesFiltered params = do
  appUuid <- asks currentAppUuid
  createDeleteEntitiesByFn entityName (appQueryUuid appUuid : params)

deleteQuestionnaireById :: String -> AppContextM Int64
deleteQuestionnaireById uuid = do
  appUuid <- asks currentAppUuid
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

enhance :: Questionnaire -> AppContextM Questionnaire
enhance qtn = do
  ps <- findQuestionnairePermRecordsFiltered [("questionnaire_uuid", U.toString qtn.uuid)]
  return $ qtn {permissions = ps}
