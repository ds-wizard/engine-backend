module Wizard.Database.DAO.Questionnaire.QuestionnaireDAO where

import Control.Monad.Reader (asks)
import Data.Foldable (traverse_)
import qualified Data.List as L
import Data.String (fromString)
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.PageMetadata
import Shared.Common.Model.Common.Pageable
import Shared.Common.Model.Common.Sort
import Shared.Common.Util.Logger
import Shared.Common.Util.String (f'', replace, trim)
import Wizard.Api.Resource.User.UserDTO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Questionnaire.QuestionnairePermDAO (
  deleteQuestionnairePermsFiltered,
  findQuestionnairePermsFiltered,
  insertQuestionnairePerm,
 )
import Wizard.Database.Mapping.Questionnaire.Questionnaire ()
import Wizard.Database.Mapping.Questionnaire.QuestionnaireDetail ()
import Wizard.Database.Mapping.Questionnaire.QuestionnaireDetailPreview ()
import Wizard.Database.Mapping.Questionnaire.QuestionnaireDetailQuestionnaire ()
import Wizard.Database.Mapping.Questionnaire.QuestionnaireDetailSettings ()
import Wizard.Database.Mapping.Questionnaire.QuestionnaireEvent ()
import Wizard.Database.Mapping.Questionnaire.QuestionnaireList ()
import Wizard.Database.Mapping.Questionnaire.QuestionnaireSimple ()
import Wizard.Database.Mapping.Questionnaire.QuestionnaireSimpleWithPerm ()
import Wizard.Database.Mapping.Questionnaire.QuestionnaireSquash ()
import Wizard.Database.Mapping.Questionnaire.QuestionnaireSuggestion ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.AppContextHelpers
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireDetail
import Wizard.Model.Questionnaire.QuestionnaireDetailPreview
import Wizard.Model.Questionnaire.QuestionnaireDetailQuestionnaire
import Wizard.Model.Questionnaire.QuestionnaireDetailSettings
import Wizard.Model.Questionnaire.QuestionnaireEvent
import Wizard.Model.Questionnaire.QuestionnaireList
import Wizard.Model.Questionnaire.QuestionnaireSimple
import Wizard.Model.Questionnaire.QuestionnaireSimpleWithPerm
import Wizard.Model.Questionnaire.QuestionnaireSquash
import Wizard.Model.Questionnaire.QuestionnaireSuggestion
import Wizard.Model.User.User

entityName = "questionnaire"

pageLabel = "questionnaires"

findQuestionnaires :: AppContextM [Questionnaire]
findQuestionnaires = do
  tenantUuid <- asks currentTenantUuid
  currentUser <- getCurrentUser
  if currentUser.uRole == _USER_ROLE_ADMIN
    then createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid] >>= traverse enhance
    else do
      let sql = f' (qtnSelectSql (U.toString tenantUuid) (U.toString $ currentUser.uuid) "['VIEW']") [""]
      logInfoI _CMP_DATABASE sql
      let action conn = query_ conn (fromString sql)
      entities <- runDB action
      traverse enhance entities

findQuestionnairesForCurrentUserPage :: Maybe String -> Maybe Bool -> Maybe Bool -> Maybe [String] -> Maybe String -> Maybe [String] -> Maybe String -> Maybe [String] -> Maybe String -> Pageable -> [Sort] -> AppContextM (Page QuestionnaireList)
findQuestionnairesForCurrentUserPage mQuery mIsTemplate mIsMigrating mProjectTags mProjectTagsOp mUserUuids mUserUuidsOp mPackageIds mPackageIdsOp pageable sort =
  -- 1. Prepare variables
  do
    tenantUuid <- asks currentTenantUuid
    currentUser <- getCurrentUser
    let (nameCondition, nameRegex) =
          case mQuery of
            Just query -> (" AND qtn.name ~* ?", [regex query])
            Nothing -> ("", [])
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
    let qtnMigrationJoin =
          case mIsMigrating of
            Nothing -> ""
            Just _ -> "LEFT JOIN questionnaire_migration qtn_mig ON qtn.uuid = qtn_mig.new_questionnaire_uuid "
    let (projectTagsCondition, projectTagsParam) =
          case mProjectTags of
            Nothing -> ("", [])
            Just [] -> ("", [])
            Just projectTags ->
              let mapFn _ = " qtn.project_tags @> ARRAY [?]"
               in if isAndOperator mProjectTagsOp
                    then (" AND (" ++ L.intercalate " AND " (fmap mapFn projectTags) ++ ")", projectTags)
                    else (" AND (" ++ L.intercalate " OR " (fmap mapFn projectTags) ++ ")", projectTags)
    let userUuidsJoin =
          case mUserUuids of
            Nothing -> ""
            Just [] -> ""
            Just _ -> "LEFT JOIN questionnaire_perm_user qtn_acl_user ON qtn.uuid = qtn_acl_user.questionnaire_uuid "
    let (userUuidsCondition, userUuidsParam) =
          case mUserUuids of
            Nothing -> ("", [])
            Just [] -> ("", [])
            Just userUuids ->
              if isAndOperator mUserUuidsOp
                then
                  ( f'
                      " AND %s = ( \
                      \SELECT COUNT(DISTINCT user_uuid) \
                      \FROM questionnaire_perm_user \
                      \WHERE questionnaire_uuid = qtn.uuid AND user_uuid in (%s)) "
                      [show . length $ userUuids, generateQuestionMarks userUuids]
                  , userUuids
                  )
                else
                  let mapFn _ = " qtn_acl_user.user_uuid = ? "
                   in (" AND (" ++ L.intercalate " OR " (fmap mapFn userUuids) ++ ")", userUuids)
    let (packageCondition, packageIdsParam) =
          case mPackageIds of
            Nothing -> ("", [])
            Just [] -> ("", [])
            Just packageIds ->
              let operator = if isAndOperator mPackageIdsOp then " AND " else " OR "
               in ( f' " AND (%s)" [L.intercalate operator . fmap (const " qtn.package_id LIKE ?") $ packageIds]
                  , fmap (replace "all" "%") packageIds
                  )
    let (aclJoins, aclCondition) =
          if currentUser.uRole == _USER_ROLE_ADMIN
            then (userUuidsJoin, "")
            else
              ( f''
                  "LEFT JOIN questionnaire_perm_user qtn_acl_user ON qtn.uuid = qtn_acl_user.questionnaire_uuid AND qtn_acl_user.tenant_uuid = '${tenantUuid}' \
                  \LEFT JOIN questionnaire_perm_group qtn_acl_group ON qtn.uuid = qtn_acl_group.questionnaire_uuid AND qtn_acl_group.tenant_uuid = '${tenantUuid}' \
                  \LEFT JOIN user_group_membership ugm ON ugm.user_group_uuid = qtn_acl_group.user_group_uuid AND ugm.user_uuid = '${currentUserUuid}' AND ugm.tenant_uuid = '${tenantUuid}'"
                  [ ("currentUserUuid", U.toString currentUser.uuid)
                  , ("tenantUuid", U.toString tenantUuid)
                  ]
              , f'
                  "AND (visibility = 'VisibleEditQuestionnaire' \
                  \  OR visibility = 'VisibleCommentQuestionnaire' \
                  \  OR visibility = 'VisibleViewQuestionnaire' \
                  \  OR (visibility = 'PrivateQuestionnaire' AND qtn_acl_user.user_uuid = '%s' AND qtn_acl_user.perms @> ARRAY %s) \
                  \  OR (visibility = 'PrivateQuestionnaire' AND qtn_acl_group.user_group_uuid = ugm.user_group_uuid AND qtn_acl_group.perms @> ARRAY %s) \
                  \)"
                  [U.toString currentUser.uuid, "['VIEW']", "['VIEW']"]
              )
    let (sizeI, pageI, skip, limit) = preparePaginationVariables pageable
    -- 2. Get total count
    let countSql =
          fromString $
            f''
              "SELECT COUNT(DISTINCT qtn.uuid) \
              \FROM questionnaire qtn \
              \${qtnMigrationJoin} \
              \${aclJoins} \
              \WHERE qtn.tenant_uuid = '${tenantUuid}' ${aclCondition} ${nameCondition} ${isTemplateCondition} ${isMigratingCondition} ${projectTagsCondition} ${userUuidsCondition} ${packageCondition}"
              [ ("qtnMigrationJoin", qtnMigrationJoin)
              , ("aclJoins", aclJoins)
              , ("tenantUuid", U.toString tenantUuid)
              , ("aclCondition", aclCondition)
              , ("nameCondition", nameCondition)
              , ("isTemplateCondition", isTemplateCondition)
              , ("isMigratingCondition", isMigratingCondition)
              , ("projectTagsCondition", projectTagsCondition)
              , ("userUuidsCondition", userUuidsCondition)
              , ("packageCondition", packageCondition)
              ]
    let params = nameRegex ++ projectTagsParam ++ userUuidsParam ++ packageIdsParam
    logQuery countSql params
    let action conn = query conn countSql params
    result <- runDB action
    let count =
          case result of
            [count] -> fromOnly count
            _ -> 0
    -- 3. Get entities
    let sql =
          fromString $
            f''
              "SELECT \
              \  nested_qtn.*, \
              \  ( \
              \    SELECT array_agg(CONCAT(qtn_acl_user.user_uuid, '::', qtn_acl_user.perms, '::', u.uuid, '::', u.first_name, '::', u.last_name, '::', u.email, '::', u.image_url)) \
              \    FROM questionnaire_perm_user qtn_acl_user \
              \    JOIN user_entity u on u.uuid = qtn_acl_user.user_uuid \
              \    WHERE questionnaire_uuid = nested_qtn.uuid \
              \    GROUP BY questionnaire_uuid \
              \  ) as user_permissions, \
              \  ( \
              \    SELECT array_agg(CONCAT(qtn_acl_group.user_group_uuid, '::', qtn_acl_group.perms, '::', ug.uuid, '::', ug.name, '::', ug.private, '::', ug.description)) \
              \    FROM questionnaire_perm_group qtn_acl_group \
              \    JOIN user_group ug on ug.uuid = qtn_acl_group.user_group_uuid \
              \    WHERE questionnaire_uuid = nested_qtn.uuid \
              \    GROUP BY questionnaire_uuid \
              \  ) as group_permissions \
              \FROM ( \
              \  SELECT DISTINCT qtn.uuid, \
              \    qtn.name, \
              \    qtn.description, \
              \    qtn.visibility, \
              \    qtn.sharing, \
              \    qtn.is_template, \
              \    qtn.created_at, \
              \    qtn.updated_at, \
              \    CASE \
              \      WHEN qtn_mig.new_questionnaire_uuid IS NOT NULL THEN 'Migrating' \
              \      WHEN qtn.package_id != get_newest_package(pkg.organization_id, pkg.km_id, '${tenantUuid}', ARRAY['ReleasedPackagePhase']) THEN 'Outdated' \
              \      WHEN qtn_mig.new_questionnaire_uuid IS NULL THEN 'Default' \
              \    END, \
              \    pkg.id, \
              \    pkg.name, \
              \    pkg.version \
              \  FROM questionnaire qtn \
              \  JOIN package pkg ON qtn.package_id = pkg.id AND qtn.tenant_uuid = pkg.tenant_uuid \
              \  LEFT JOIN questionnaire_migration qtn_mig ON qtn.uuid = qtn_mig.new_questionnaire_uuid \
              \  ${aclJoins} \
              \  WHERE qtn.tenant_uuid = '${tenantUuid}' ${aclCondition} ${nameCondition} ${isTemplateCondition} ${isMigratingCondition} ${projectTagsCondition} ${userUuidsCondition} ${packageCondition} \
              \  ${sort} \
              \  OFFSET ${offset} LIMIT ${limit} \
              \) nested_qtn"
              [ ("aclJoins", aclJoins)
              , ("tenantUuid", U.toString tenantUuid)
              , ("aclCondition", aclCondition)
              , ("nameCondition", nameCondition)
              , ("isTemplateCondition", isTemplateCondition)
              , ("isMigratingCondition", isMigratingCondition)
              , ("projectTagsCondition", projectTagsCondition)
              , ("userUuidsCondition", userUuidsCondition)
              , ("packageCondition", packageCondition)
              , ("sort", mapSortWithPrefix "qtn" sort)
              , ("offset", show skip)
              , ("limit", show sizeI)
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
  tenantUuid <- asks currentTenantUuid
  currentUser <- getCurrentUser
  if currentUser.uRole == _USER_ROLE_ADMIN
    then createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid, ("package_id", packageId)] >>= traverse enhance
    else do
      let sql =
            fromString $
              f' (qtnSelectSql (U.toString tenantUuid) (U.toString $ currentUser.uuid) "['VIEW']") ["AND package_id = ?"]
      let params = [packageId]
      logQuery sql params
      let action conn = query conn sql params
      entities <- runDB action
      traverse enhance entities

findQuestionnairesByDocumentTemplateId :: String -> AppContextM [Questionnaire]
findQuestionnairesByDocumentTemplateId documentTemplateId = do
  tenantUuid <- asks currentTenantUuid
  currentUser <- getCurrentUser
  if currentUser.uRole == _USER_ROLE_ADMIN
    then createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid, ("document_template_id", documentTemplateId)] >>= traverse enhance
    else do
      let sql =
            fromString $
              f' (qtnSelectSql (U.toString tenantUuid) (U.toString $ currentUser.uuid) "['VIEW']") ["AND document_template_id = ?"]
      let params = [documentTemplateId]
      logQuery sql params
      let action conn = query conn sql params
      entities <- runDB action
      traverse enhance entities

findQuestionnaireWithZeroAcl :: AppContextM [Questionnaire]
findQuestionnaireWithZeroAcl = do
  let sql =
        f'
          "SELECT qtn.* \
          \FROM %s qtn \
          \LEFT JOIN questionnaire_perm_user qtn_acl_user ON qtn.uuid = qtn_acl_user.questionnaire_uuid \
          \LEFT JOIN questionnaire_perm_group qtn_acl_group ON qtn.uuid = qtn_acl_group.questionnaire_uuid \
          \WHERE qtn_acl_user.user_uuid IS NULL \
          \AND qtn_acl_group.user_group_uuid IS NULL \
          \AND qtn.updated_at < now() - INTERVAL '30 days'"
          [entityName]
  logInfoI _CMP_DATABASE (trim sql)
  let action conn = query_ conn (fromString sql)
  runDB action

findQuestionnairesSimpleWithPermByUserGroupUuid :: U.UUID -> AppContextM [QuestionnaireSimpleWithPerm]
findQuestionnairesSimpleWithPermByUserGroupUuid userGroupUuid = do
  tenantUuid <- asks currentTenantUuid
  let sql =
        fromString
          "SELECT \
          \  nested_qtn.*, \
          \  ( \
          \    SELECT array_agg(CONCAT(user_uuid, '::', perms)) \
          \    FROM questionnaire_perm_user qtn_acl_user \
          \    WHERE questionnaire_uuid = nested_qtn.uuid AND tenant_uuid = nested_qtn.tenant_uuid \
          \    GROUP BY questionnaire_uuid \
          \  ) as user_permissions, \
          \  ( \
          \    SELECT array_agg(CONCAT(user_group_uuid, '::', perms)) \
          \    FROM questionnaire_perm_group qtn_acl_group \
          \    WHERE questionnaire_uuid = nested_qtn.uuid AND tenant_uuid = nested_qtn.tenant_uuid \
          \    GROUP BY questionnaire_uuid \
          \  ) as group_permissions \
          \FROM ( \
          \  SELECT qtn.uuid, qtn.visibility, qtn.sharing, qtn.tenant_uuid \
          \  FROM questionnaire qtn \
          \  LEFT JOIN questionnaire_perm_group qtn_perm_group ON qtn.uuid = qtn_perm_group.questionnaire_uuid AND qtn.tenant_uuid = qtn_perm_group.tenant_uuid \
          \  WHERE qtn_perm_group.user_group_uuid = ? AND qtn_perm_group.tenant_uuid = ? \
          \) nested_qtn"
  let params = [toField userGroupUuid, toField tenantUuid]
  logQuery sql params
  let action conn = query conn sql params
  runDB action

findQuestionnaireUuids :: AppContextM [U.UUID]
findQuestionnaireUuids = do
  tenantUuid <- asks currentTenantUuid
  let sql = fromString $ f' "SELECT %s FROM %s WHERE tenant_uuid = ?" ["uuid", entityName]
  let params = [toField tenantUuid]
  logQuery sql params
  let action conn = query conn sql params
  entities <- runDB action
  return . concat $ entities

findQuestionnaireUuids' :: AppContextM [U.UUID]
findQuestionnaireUuids' = do
  let sql = f' "SELECT %s FROM %s" ["uuid", entityName]
  logInfoI _CMP_DATABASE (trim sql)
  let action conn = query_ conn (fromString sql)
  entities <- runDB action
  return . concat $ entities

findQuestionnaireByUuid :: U.UUID -> AppContextM Questionnaire
findQuestionnaireByUuid qtnUuid = do
  tenantUuid <- asks currentTenantUuid
  entity <- createFindEntityByFn entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString qtnUuid)]
  enhance entity

findQuestionnaireByUuid' :: U.UUID -> AppContextM (Maybe Questionnaire)
findQuestionnaireByUuid' qtnUuid = do
  tenantUuid <- asks currentTenantUuid
  mEntity <- createFindEntityByFn' entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString qtnUuid)]
  case mEntity of
    Just entity -> enhance entity >>= return . Just
    Nothing -> return Nothing

findQuestionnaireSimpleByUuid :: U.UUID -> AppContextM QuestionnaireSimple
findQuestionnaireSimpleByUuid uuid = do
  tenantUuid <- asks currentTenantUuid
  createFindEntityWithFieldsByFn "uuid, name" False entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]

findQuestionnaireSimpleByUuid' :: U.UUID -> AppContextM (Maybe QuestionnaireSimple)
findQuestionnaireSimpleByUuid' uuid = do
  tenantUuid <- asks currentTenantUuid
  createFindEntityWithFieldsByFn' "uuid, name" entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]

findQuestionnaireSuggestionByUuid' :: U.UUID -> AppContextM (Maybe QuestionnaireSuggestion)
findQuestionnaireSuggestionByUuid' uuid = do
  tenantUuid <- asks currentTenantUuid
  createFindEntityWithFieldsByFn' "uuid, name, description" entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]

findQuestionnaireEventsByUuid :: U.UUID -> AppContextM [QuestionnaireEvent]
findQuestionnaireEventsByUuid uuid = do
  tenantUuid <- asks currentTenantUuid
  let sql = fromString "SELECT events FROM questionnaire qtn WHERE tenant_uuid = ? AND uuid = ?"
  let params = [toField tenantUuid, toField uuid]
  logQuery sql params
  let action conn = query conn sql params
  entities <- runDB action :: AppContextM [QuestionnaireEventBundle]
  case entities of
    [entity] -> return entity.events
    _ -> return []

findQuestionnaireForSquashing :: AppContextM [U.UUID]
findQuestionnaireForSquashing = do
  let sql = "SELECT uuid FROM questionnaire WHERE squashed = false"
  logInfoI _CMP_DATABASE (trim sql)
  let action conn = query_ conn (fromString sql)
  entities <- runDB action
  return . concat $ entities

findQuestionnaireSquashByUuid :: U.UUID -> AppContextM QuestionnaireSquash
findQuestionnaireSquashByUuid uuid =
  createFindEntityWithFieldsByFn "uuid, events, versions" True entityName [("uuid", U.toString uuid)]

findQuestionnaireDetail :: U.UUID -> AppContextM QuestionnaireDetail
findQuestionnaireDetail uuid = do
  tenantUuid <- asks currentTenantUuid
  let sql =
        fromString $
          f''
            "SELECT qtn.uuid, \
            \       qtn.name, \
            \       qtn.visibility, \
            \       qtn.sharing, \
            \       qtn.package_id, \
            \       qtn.selected_question_tag_uuids, \
            \       qtn.is_template, \
            \       qtn.events, \
            \       qtn_mig.new_questionnaire_uuid AS migration_uuid, \
            \       ${questionnaireDetailPermSql}, \
            \       ( \
            \        SELECT count(*) \
            \        FROM questionnaire_action \
            \        WHERE tenant_uuid = '${tenantUuid}' \
            \       ) as questionnaire_actions, \
            \       ( \
            \        SELECT count(*) \
            \        FROM questionnaire_importer \
            \        WHERE tenant_uuid = '${tenantUuid}' \
            \       ) as questionnaire_impoters, \
            \       ( \
            \        SELECT count(*) \
            \        FROM questionnaire_file \
            \        WHERE tenant_uuid = '${tenantUuid}' AND questionnaire_uuid = '${questionnaireUuid}' \
            \       ) as file_count \
            \FROM questionnaire qtn \
            \LEFT JOIN questionnaire_migration qtn_mig ON qtn.uuid = qtn_mig.old_questionnaire_uuid AND qtn.tenant_uuid = qtn_mig.tenant_uuid \
            \WHERE qtn.tenant_uuid = ? AND qtn.uuid = ?"
            [ ("questionnaireDetailPermSql", questionnaireDetailPermSql)
            , ("questionnaireUuid", U.toString uuid)
            , ("tenantUuid", U.toString tenantUuid)
            ]
  let queryParams = [("tenant_uuid", U.toString tenantUuid), ("uuid", U.toString uuid)]
  let params = fmap snd queryParams
  logQuery sql params
  let action conn = query conn sql params
  runOneEntityDB entityName action queryParams

findQuestionnaireDetailQuestionnaire :: U.UUID -> AppContextM QuestionnaireDetailQuestionnaire
findQuestionnaireDetailQuestionnaire uuid = do
  tenantUuid <- asks currentTenantUuid
  let sql =
        fromString $
          f''
            "SELECT qtn.uuid, \
            \       qtn.name, \
            \       qtn.visibility, \
            \       qtn.sharing, \
            \       qtn.package_id, \
            \       qtn.selected_question_tag_uuids, \
            \       qtn.is_template, \
            \       qtn.events, \
            \       qtn_mig.new_questionnaire_uuid AS migration_uuid, \
            \       ${questionnaireDetailPermSql}, \
            \       ( \
            \        SELECT count(*) \
            \        FROM questionnaire_action \
            \        WHERE tenant_uuid = '${tenantUuid}' \
            \       ) as questionnaire_actions, \
            \       ( \
            \        SELECT count(*) \
            \        FROM questionnaire_importer \
            \        WHERE tenant_uuid = '${tenantUuid}' \
            \       ) as questionnaire_impoters, \
            \       ( \
            \        SELECT array_agg(concat(uuid, '<:::::>', \
            \                                file_name, '<:::::>', \
            \                                content_type, '<:::::>', \
            \                                file_size \
            \                        )) \
            \        FROM questionnaire_file \
            \        WHERE tenant_uuid = '${tenantUuid}' AND questionnaire_uuid = '${questionnaireUuid}' \
            \       ) as files \
            \FROM questionnaire qtn \
            \LEFT JOIN questionnaire_migration qtn_mig ON qtn.uuid = qtn_mig.old_questionnaire_uuid AND qtn.tenant_uuid = qtn_mig.tenant_uuid \
            \WHERE qtn.tenant_uuid = ? AND qtn.uuid = ?"
            [ ("questionnaireUuid", U.toString uuid)
            , ("questionnaireDetailPermSql", questionnaireDetailPermSql)
            , ("tenantUuid", U.toString tenantUuid)
            ]
  let queryParams = [("tenant_uuid", U.toString tenantUuid), ("uuid", U.toString uuid)]
  let params = fmap snd queryParams
  logQuery sql params
  let action conn = query conn sql params
  runOneEntityDB entityName action queryParams

findQuestionnaireDetailPreview :: U.UUID -> AppContextM QuestionnaireDetailPreview
findQuestionnaireDetailPreview uuid = do
  tenantUuid <- asks currentTenantUuid
  let sql =
        fromString $
          f''
            "SELECT qtn.uuid, \
            \       qtn.name, \
            \       qtn.visibility, \
            \       qtn.sharing, \
            \       qtn.package_id, \
            \       qtn.is_template, \
            \       qtn.document_template_id, \
            \       qtn.format_uuid, \
            \       qtn_mig.new_questionnaire_uuid AS migration_uuid, \
            \       ${questionnaireDetailPermSql}, \
            \       dt.formats                     AS document_template_formats, \
            \       ( \
            \        SELECT count(*) \
            \        FROM questionnaire_file \
            \        WHERE tenant_uuid = '${tenantUuid}' AND questionnaire_uuid = '${questionnaireUuid}' \
            \       ) as file_count \
            \FROM questionnaire qtn \
            \LEFT JOIN questionnaire_migration qtn_mig ON qtn.uuid = qtn_mig.old_questionnaire_uuid AND qtn.tenant_uuid = qtn_mig.tenant_uuid \
            \LEFT JOIN document_template dt ON qtn.document_template_id = dt.id AND qtn.tenant_uuid = dt.tenant_uuid \
            \WHERE qtn.tenant_uuid = ? AND qtn.uuid = ?"
            [ ("questionnaireDetailPermSql", questionnaireDetailPermSql)
            , ("questionnaireUuid", U.toString uuid)
            , ("tenantUuid", U.toString tenantUuid)
            ]
  let queryParams = [("tenant_uuid", U.toString tenantUuid), ("uuid", U.toString uuid)]
  let params = fmap snd queryParams
  logQuery sql params
  let action conn = query conn sql params
  runOneEntityDB entityName action queryParams

findQuestionnaireDetailSettings :: U.UUID -> AppContextM QuestionnaireDetailSettings
findQuestionnaireDetailSettings uuid = do
  tenantUuid <- asks currentTenantUuid
  let sql =
        fromString $
          f''
            "SELECT qtn.uuid, \
            \       qtn.name, \
            \       qtn.description, \
            \       qtn.visibility, \
            \       qtn.sharing, \
            \       qtn.is_template, \
            \       qtn.project_tags, \
            \       qtn.selected_question_tag_uuids, \
            \       qtn.format_uuid, \
            \       qtn_mig.new_questionnaire_uuid AS migration_uuid, \
            \       ${questionnaireDetailPermSql}, \
            \       pkg.id                         as package_id, \
            \       pkg.name                       as package_name, \
            \       pkg.organization_id            as package_organization_id, \
            \       pkg.km_id                      as package_km_id, \
            \       pkg.version                    as package_version, \
            \       pkg.phase                      as package_phase, \
            \       pkg.description                as package_description, \
            \       pkg.non_editable               as package_non_editable, \
            \       pkg.created_at                 as package_created_at, \
            \       dt.id                          as document_template_id, \
            \       dt.name                        as document_template_name, \
            \       dt.version                     as document_template_version, \
            \       dt.phase                       as document_template_phase, \
            \       dt.description                 as document_template_description, \
            \       dt.formats                     as document_template_formats, \
            \       dt.metamodel_version           as document_template_metamodel_version, \
            \       ( \
            \        SELECT count(*) \
            \        FROM questionnaire_file \
            \        WHERE tenant_uuid = '${tenantUuid}' AND questionnaire_uuid = '${questionnaireUuid}' \
            \       ) as file_count \
            \FROM questionnaire qtn \
            \LEFT JOIN questionnaire_migration qtn_mig ON qtn.uuid = qtn_mig.old_questionnaire_uuid AND qtn.tenant_uuid = qtn_mig.tenant_uuid \
            \LEFT JOIN package pkg ON qtn.package_id = pkg.id AND qtn.tenant_uuid = pkg.tenant_uuid \
            \LEFT JOIN document_template dt ON qtn.document_template_id = dt.id AND qtn.tenant_uuid = dt.tenant_uuid \
            \WHERE qtn.tenant_uuid = ? AND qtn.uuid = ?"
            [ ("questionnaireDetailPermSql", questionnaireDetailPermSql)
            , ("questionnaireUuid", U.toString uuid)
            , ("tenantUuid", U.toString tenantUuid)
            ]
  let queryParams = [("tenant_uuid", U.toString tenantUuid), ("uuid", U.toString uuid)]
  let params = fmap snd queryParams
  logQuery sql params
  let action conn = query conn sql params
  runOneEntityDB entityName action queryParams

questionnaireDetailPermSql :: String
questionnaireDetailPermSql =
  "(SELECT array_agg(CONCAT(qtn_acl_user.user_uuid, '::', qtn_acl_user.perms, '::', u.uuid, '::', u.first_name, \
  \                         '::', u.last_name, '::', u.email, '::', u.image_url)) \
  \ FROM questionnaire_perm_user qtn_acl_user \
  \          JOIN user_entity u on u.uuid = qtn_acl_user.user_uuid \
  \ WHERE questionnaire_uuid = qtn.uuid \
  \ GROUP BY questionnaire_uuid)  as user_permissions, \
  \(SELECT array_agg(CONCAT(qtn_acl_group.user_group_uuid, '::', qtn_acl_group.perms, '::', ug.uuid, '::', ug.name, \
  \                         '::', ug.private, '::', ug.description)) \
  \ FROM questionnaire_perm_group qtn_acl_group \
  \          JOIN user_group ug on ug.uuid = qtn_acl_group.user_group_uuid \
  \ WHERE questionnaire_uuid = qtn.uuid \
  \ GROUP BY questionnaire_uuid)  as group_permissions"

countQuestionnaires :: AppContextM Int
countQuestionnaires = do
  tenantUuid <- asks currentTenantUuid
  countQuestionnairesWithTenant tenantUuid

countQuestionnairesWithTenant :: U.UUID -> AppContextM Int
countQuestionnairesWithTenant tenantUuid = createCountByFn entityName tenantCondition [U.toString tenantUuid]

insertQuestionnaire :: Questionnaire -> AppContextM Int64
insertQuestionnaire qtn = do
  createInsertFn entityName qtn
  traverse_ insertQuestionnairePerm qtn.permissions
  return 1

updateQuestionnaireByUuid :: Questionnaire -> AppContextM ()
updateQuestionnaireByUuid qtn = do
  tenantUuid <- asks currentTenantUuid
  let sql =
        fromString
          "UPDATE questionnaire SET uuid = ?, name = ?, visibility = ?, sharing = ?, package_id = ?, selected_question_tag_uuids = ?, document_template_id = ?, format_uuid = ?, created_by = ?, events = ?, versions = ?, created_at = ?, updated_at = ?, description = ?, is_template = ?, squashed = ?, tenant_uuid = ?, project_tags = ? WHERE  tenant_uuid = ? AND uuid = ?"
  let params = toRow qtn ++ [toField tenantUuid, toField . U.toText $ qtn.uuid]
  logInsertAndUpdate sql params
  let action conn = execute conn sql params
  runDB action
  deleteQuestionnairePermsFiltered [("questionnaire_uuid", U.toString qtn.uuid)]
  traverse_ insertQuestionnairePerm qtn.permissions

updateQuestionnaireEventsByUuid :: U.UUID -> Bool -> [QuestionnaireEvent] -> AppContextM ()
updateQuestionnaireEventsByUuid qtnUuid squashed events = do
  let sql = fromString "UPDATE questionnaire SET squashed = ?, events = ?, updated_at = now() WHERE uuid = ?"
  let params = [toField squashed, toJSONField events, toField qtnUuid]
  logInsertAndUpdate sql params
  let action conn = execute conn sql params
  runDB action
  return ()

updateQuestionnaireEventsByUuid' :: U.UUID -> Bool -> [QuestionnaireEvent] -> AppContextM ()
updateQuestionnaireEventsByUuid' qtnUuid squashed events = do
  let sql = fromString "UPDATE questionnaire SET squashed = ?, events = ? WHERE uuid = ?"
  let params = [toField squashed, toJSONField events, toField qtnUuid]
  logInsertAndUpdate sql params
  let action conn = execute conn sql params
  runDB action
  return ()

updateQuestionnaireEventsByUuid'' :: U.UUID -> [QuestionnaireEvent] -> AppContextM ()
updateQuestionnaireEventsByUuid'' qtnUuid events = do
  let sql = fromString "UPDATE questionnaire SET events = ? WHERE uuid = ?"
  let params = [toJSONField events, toField qtnUuid]
  logInsertAndUpdate sql params
  let action conn = execute conn sql params
  runDB action
  return ()

appendQuestionnaireEventByUuid :: U.UUID -> [QuestionnaireEvent] -> AppContextM ()
appendQuestionnaireEventByUuid qtnUuid events = do
  tenantUuid <- asks currentTenantUuid
  let sql =
        fromString
          "UPDATE questionnaire SET squashed = false, events = events || ?, updated_at = now() WHERE tenant_uuid = ? AND uuid = ?"
  let params =
        [ toJSONField events
        , toField tenantUuid
        , toField qtnUuid
        ]
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
  tenantUuid <- asks currentTenantUuid
  createDeleteEntitiesByFn entityName (tenantQueryUuid tenantUuid : params)

deleteQuestionnaireByUuid :: U.UUID -> AppContextM Int64
deleteQuestionnaireByUuid uuid = do
  tenantUuid <- asks currentTenantUuid
  createDeleteEntityByFn entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]

-- ------------------------------------------------------------------------------------------------------------------------------
-- PRIVATE
-- ------------------------------------------------------------------------------------------------------------------------------
qtnSelectSql tenantUuid userUuid perm =
  f'
    "SELECT qtn.* \
    \FROM questionnaire qtn \
    \LEFT JOIN questionnaire_perm_user qtn_acl_user ON qtn.uuid = qtn_acl_user.questionnaire_uuid \
    \LEFT JOIN questionnaire_perm_group qtn_acl_group ON qtn.uuid = qtn_acl_group.questionnaire_uuid \
    \WHERE %s %s"
    [qtnWhereSql tenantUuid userUuid perm]

qtnWhereSql tenantUuid userUuid perm =
  f'
    "qtn.tenant_uuid = '%s' \
    \AND (visibility = 'VisibleEditQuestionnaire' \
    \OR visibility = 'VisibleCommentQuestionnaire' \
    \OR visibility = 'VisibleViewQuestionnaire' \
    \OR (visibility = 'PrivateQuestionnaire' AND qtn_acl_user.user_uuid = '%s' AND qtn_acl_user.perms @> ARRAY %s))"
    [tenantUuid, userUuid, perm]

enhance :: Questionnaire -> AppContextM Questionnaire
enhance qtn = do
  ps <- findQuestionnairePermsFiltered [("questionnaire_uuid", U.toString qtn.uuid)]
  return $ qtn {permissions = ps}
