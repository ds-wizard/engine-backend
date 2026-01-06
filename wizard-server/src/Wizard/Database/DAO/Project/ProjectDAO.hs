module Wizard.Database.DAO.Project.ProjectDAO where

import Control.Monad.Reader (asks)
import Data.Foldable (traverse_)
import qualified Data.List as L
import Data.String (fromString)
import Data.Time
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
import Wizard.Constant.ProjectAction
import Wizard.Constant.ProjectImporter
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Project.ProjectPermDAO (
  deleteProjectPermsFiltered,
  findProjectPermsFiltered,
  insertProjectPerm,
 )
import Wizard.Database.Mapping.Project.Project ()
import Wizard.Database.Mapping.Project.ProjectDetail ()
import Wizard.Database.Mapping.Project.ProjectDetailPreview ()
import Wizard.Database.Mapping.Project.ProjectDetailQuestionnaire ()
import Wizard.Database.Mapping.Project.ProjectDetailSettings ()
import Wizard.Database.Mapping.Project.ProjectList ()
import Wizard.Database.Mapping.Project.ProjectSimple ()
import Wizard.Database.Mapping.Project.ProjectSimpleWithPerm ()
import Wizard.Database.Mapping.Project.ProjectSuggestion ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.AppContextHelpers
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.Project.Detail.ProjectDetail
import Wizard.Model.Project.Detail.ProjectDetailPreview
import Wizard.Model.Project.Detail.ProjectDetailQuestionnaire
import Wizard.Model.Project.Detail.ProjectDetailSettings
import Wizard.Model.Project.Project
import Wizard.Model.Project.ProjectList
import Wizard.Model.Project.ProjectSimpleWithPerm
import Wizard.Model.Project.ProjectSuggestion
import Wizard.Model.User.User

entityName = "project"

pageLabel = "projects"

findProjects :: AppContextM [Project]
findProjects = do
  tenantUuid <- asks currentTenantUuid
  currentUser <- getCurrentUser
  if currentUser.uRole == _USER_ROLE_ADMIN
    then createFindEntitiesBySortedFn entityName [tenantQueryUuid tenantUuid] [Sort "name" Ascending] >>= traverse enhance
    else do
      let sql = f' (projectSelectSql (U.toString tenantUuid) (U.toString $ currentUser.uuid) "['VIEW']") [""] ++ " ORDER BY project.name ASC"
      logInfoI _CMP_DATABASE sql
      let action conn = query_ conn (fromString sql)
      entities <- runDB action
      traverse enhance entities

findProjectsForCurrentUserPage :: Maybe String -> Maybe Bool -> Maybe Bool -> Maybe [String] -> Maybe String -> Maybe [String] -> Maybe String -> Maybe [String] -> Maybe String -> Pageable -> [Sort] -> AppContextM (Page ProjectList)
findProjectsForCurrentUserPage mQuery mIsTemplate mIsMigrating mProjectTags mProjectTagsOp mUserUuids mUserUuidsOp mKnowledgeModelPackageIds mKnowledgeModelPackageIdsOp pageable sort =
  -- 1. Prepare variables
  do
    tenantUuid <- asks currentTenantUuid
    currentUser <- getCurrentUser
    let (nameCondition, nameRegex) =
          case mQuery of
            Just query -> (" AND project.name ~* ?", [regex query])
            Nothing -> ("", [])
    let isTemplateCondition =
          case mIsTemplate of
            Nothing -> ""
            Just True -> " AND project.is_template = true"
            Just False -> " AND project.is_template = false"
    let isMigratingCondition useWhere =
          case mIsMigrating of
            Nothing -> ""
            Just True -> f' " %s project_mig.new_project_uuid IS NOT NULL" [if useWhere then "WHERE" else "AND"]
            Just False -> f' " %s project_mig.new_project_uuid IS NULL" [if useWhere then "WHERE" else "AND"]
    let projectMigrationJoin =
          case mIsMigrating of
            Nothing -> ""
            Just _ -> "LEFT JOIN project_migration project_mig ON project.uuid = project_mig.new_project_uuid "
    let (projectTagsCondition, projectTagsParam) =
          case mProjectTags of
            Nothing -> ("", [])
            Just [] -> ("", [])
            Just projectTags ->
              let mapFn _ = " project.project_tags @> ARRAY [?]"
               in if isAndOperator mProjectTagsOp
                    then (" AND (" ++ L.intercalate " AND " (fmap mapFn projectTags) ++ ")", projectTags)
                    else (" AND (" ++ L.intercalate " OR " (fmap mapFn projectTags) ++ ")", projectTags)
    let userUuidsJoin =
          case mUserUuids of
            Nothing -> ""
            Just [] -> ""
            Just _ -> "LEFT JOIN project_perm_user ON project.uuid = project_perm_user.project_uuid "
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
                      \FROM project_perm_user \
                      \WHERE project_uuid = project.uuid AND user_uuid in (%s)) "
                      [show . length $ userUuids, generateQuestionMarks userUuids]
                  , userUuids
                  )
                else
                  let mapFn _ = " project_perm_user.user_uuid = ? "
                   in (" AND (" ++ L.intercalate " OR " (fmap mapFn userUuids) ++ ")", userUuids)
    let (knowledgeModelPackageCondition, knowledgeModelPackageIdsParam) =
          case mKnowledgeModelPackageIds of
            Nothing -> ("", [])
            Just [] -> ("", [])
            Just packageIds ->
              let operator = if isAndOperator mKnowledgeModelPackageIdsOp then " AND " else " OR "
               in ( f' " AND (%s)" [L.intercalate operator . fmap (const " project.knowledge_model_package_id LIKE ?") $ packageIds]
                  , fmap (replace "all" "%") packageIds
                  )
    let (aclJoins, aclCondition) =
          if currentUser.uRole == _USER_ROLE_ADMIN
            then (userUuidsJoin, "")
            else
              ( f''
                  "LEFT JOIN project_perm_user ON project.uuid = project_perm_user.project_uuid AND project_perm_user.tenant_uuid = '${tenantUuid}' \
                  \LEFT JOIN project_perm_group ON project.uuid = project_perm_group.project_uuid AND project_perm_group.tenant_uuid = '${tenantUuid}' \
                  \LEFT JOIN user_group_membership ugm ON ugm.user_group_uuid = project_perm_group.user_group_uuid AND ugm.user_uuid = '${currentUserUuid}' AND ugm.tenant_uuid = '${tenantUuid}'"
                  [ ("currentUserUuid", U.toString currentUser.uuid)
                  , ("tenantUuid", U.toString tenantUuid)
                  ]
              , f'
                  "AND (visibility = 'VisibleEditProjectVisibility' \
                  \  OR visibility = 'VisibleCommentProjectVisibility' \
                  \  OR visibility = 'VisibleViewProjectVisibility' \
                  \  OR (visibility = 'PrivateProjectVisibility' AND project_perm_user.user_uuid = '%s' AND project_perm_user.perms @> ARRAY %s) \
                  \  OR (visibility = 'PrivateProjectVisibility' AND project_perm_group.user_group_uuid = ugm.user_group_uuid AND project_perm_group.perms @> ARRAY %s) \
                  \)"
                  [U.toString currentUser.uuid, "['VIEW']", "['VIEW']"]
              )
    let (sizeI, pageI, skip, limit) = preparePaginationVariables pageable
    -- 2. Get total count
    let countSql =
          fromString $
            f''
              "SELECT COUNT(DISTINCT project.uuid) \
              \FROM project \
              \${projectMigrationJoin} \
              \${aclJoins} \
              \WHERE project.tenant_uuid = '${tenantUuid}' ${aclCondition} ${nameCondition} ${isTemplateCondition} ${isMigratingCondition} ${projectTagsCondition} ${userUuidsCondition} ${knowledgeModelPackageCondition}"
              [ ("projectMigrationJoin", projectMigrationJoin)
              , ("aclJoins", aclJoins)
              , ("tenantUuid", U.toString tenantUuid)
              , ("aclCondition", aclCondition)
              , ("nameCondition", nameCondition)
              , ("isTemplateCondition", isTemplateCondition)
              , ("isMigratingCondition", isMigratingCondition False)
              , ("projectTagsCondition", projectTagsCondition)
              , ("userUuidsCondition", userUuidsCondition)
              , ("knowledgeModelPackageCondition", knowledgeModelPackageCondition)
              ]
    let params = nameRegex ++ projectTagsParam ++ userUuidsParam ++ knowledgeModelPackageIdsParam
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
              "WITH filtered_project AS (SELECT DISTINCT project.uuid, \
              \                             project.name, \
              \                             project.description, \
              \                             project.visibility, \
              \                             project.sharing, \
              \                             project.is_template, \
              \                             project.created_at, \
              \                             project.updated_at, \
              \                             project.knowledge_model_package_id \
              \             FROM project \
              \             ${aclJoins} \
              \             WHERE project.tenant_uuid = '${tenantUuid}' ${aclCondition} ${nameCondition} ${isTemplateCondition} ${projectTagsCondition} ${userUuidsCondition} ${knowledgeModelPackageCondition}), \
              \     pkg AS (SELECT knowledge_model_package.id, \
              \                    knowledge_model_package.name, \
              \                    knowledge_model_package.version, \
              \                    knowledge_model_package.organization_id, \
              \                    knowledge_model_package.km_id \
              \             FROM knowledge_model_package \
              \             WHERE knowledge_model_package.tenant_uuid = '${tenantUuid}'), \
              \     project_mig AS (SELECT new_project_uuid \
              \                 FROM project_migration \
              \                 WHERE project_migration.tenant_uuid = '${tenantUuid}') \
              \SELECT  filtered_project.uuid, \
              \        filtered_project.name, \
              \        filtered_project.description, \
              \        filtered_project.visibility, \
              \        filtered_project.sharing, \
              \        filtered_project.is_template, \
              \        filtered_project.created_at, \
              \        filtered_project.updated_at, \
              \        CASE \
              \          WHEN project_mig.new_project_uuid IS NOT NULL THEN 'MigratingProjectState' \
              \          WHEN filtered_project.knowledge_model_package_id != get_newest_knowledge_model_package(pkg.organization_id, pkg.km_id, '${tenantUuid}', ARRAY['ReleasedKnowledgeModelPackagePhase']) THEN 'OutdatedProjectState' \
              \          WHEN project_mig.new_project_uuid IS NULL THEN 'DefaultProjectState' END, \
              \        pkg.id, \
              \        pkg.name, \
              \        pkg.version, \
              \       (SELECT array_agg(CONCAT(project_perm_user.user_uuid, '::', project_perm_user.perms, '::', u.uuid, '::', u.first_name, '::', u.last_name, '::', u.email, '::', u.image_url)) \
              \        FROM project_perm_user \
              \        JOIN user_entity u on u.uuid = project_perm_user.user_uuid \
              \        WHERE project_uuid = filtered_project.uuid \
              \        GROUP BY project_uuid) as user_permissions, \
              \       (SELECT array_agg(CONCAT(project_perm_group.user_group_uuid, '::', project_perm_group.perms, '::', ug.uuid, '::', ug.name, '::', ug.private, '::', ug.description)) \
              \        FROM project_perm_group \
              \        JOIN user_group ug on ug.uuid = project_perm_group.user_group_uuid \
              \        WHERE project_uuid = filtered_project.uuid \
              \        GROUP BY project_uuid) as group_permissions \
              \FROM filtered_project \
              \JOIN pkg ON filtered_project.knowledge_model_package_id = pkg.id \
              \LEFT JOIN project_mig ON filtered_project.uuid = project_mig.new_project_uuid \
              \${isMigratingCondition} \
              \${sort} \
              \OFFSET ${offset} LIMIT ${limit}"
              [ ("aclJoins", aclJoins)
              , ("tenantUuid", U.toString tenantUuid)
              , ("aclCondition", aclCondition)
              , ("nameCondition", nameCondition)
              , ("isTemplateCondition", isTemplateCondition)
              , ("isMigratingCondition", isMigratingCondition True)
              , ("projectTagsCondition", projectTagsCondition)
              , ("userUuidsCondition", userUuidsCondition)
              , ("knowledgeModelPackageCondition", knowledgeModelPackageCondition)
              , ("sort", mapSortWithPrefix "filtered_project" sort)
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

findProjectsByKnowledgeModelPackageId :: String -> AppContextM [Project]
findProjectsByKnowledgeModelPackageId packageId = do
  tenantUuid <- asks currentTenantUuid
  currentUser <- getCurrentUser
  if currentUser.uRole == _USER_ROLE_ADMIN
    then createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid, ("knowledge_model_package_id", packageId)] >>= traverse enhance
    else do
      let sql =
            fromString $
              f' (projectSelectSql (U.toString tenantUuid) (U.toString $ currentUser.uuid) "['VIEW']") ["AND knowledge_model_package_id = ?"]
      let params = [packageId]
      logQuery sql params
      let action conn = query conn sql params
      entities <- runDB action
      traverse enhance entities

findProjectsByDocumentTemplateId :: String -> AppContextM [Project]
findProjectsByDocumentTemplateId documentTemplateId = do
  tenantUuid <- asks currentTenantUuid
  currentUser <- getCurrentUser
  if currentUser.uRole == _USER_ROLE_ADMIN
    then createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid, ("document_template_id", documentTemplateId)] >>= traverse enhance
    else do
      let sql =
            fromString $
              f' (projectSelectSql (U.toString tenantUuid) (U.toString $ currentUser.uuid) "['VIEW']") ["AND document_template_id = ?"]
      let params = [documentTemplateId]
      logQuery sql params
      let action conn = query conn sql params
      entities <- runDB action
      traverse enhance entities

findProjectsWithZeroAcl :: AppContextM [Project]
findProjectsWithZeroAcl = do
  let sql =
        f'
          "SELECT project.* \
          \FROM %s \
          \LEFT JOIN project_perm_user ON project.uuid = project_perm_user.project_uuid \
          \LEFT JOIN project_perm_group ON project.uuid = project_perm_group.project_uuid \
          \WHERE project_perm_user.user_uuid IS NULL \
          \AND project_perm_group.user_group_uuid IS NULL \
          \AND project.updated_at < now() - INTERVAL '30 days'"
          [entityName]
  logInfoI _CMP_DATABASE (trim sql)
  let action conn = query_ conn (fromString sql)
  runDB action

findProjectsSimpleWithPermByUserGroupUuid :: U.UUID -> AppContextM [ProjectSimpleWithPerm]
findProjectsSimpleWithPermByUserGroupUuid userGroupUuid = do
  tenantUuid <- asks currentTenantUuid
  let sql =
        fromString
          "SELECT \
          \  nested_project.*, \
          \  ( \
          \    SELECT array_agg(CONCAT(user_uuid, '::', perms)) \
          \    FROM project_perm_user \
          \    WHERE project_uuid = nested_project.uuid AND tenant_uuid = nested_project.tenant_uuid \
          \    GROUP BY project_uuid \
          \  ) as user_permissions, \
          \  ( \
          \    SELECT array_agg(CONCAT(user_group_uuid, '::', perms)) \
          \    FROM project_perm_group \
          \    WHERE project_uuid = nested_project.uuid AND tenant_uuid = nested_project.tenant_uuid \
          \    GROUP BY project_uuid \
          \  ) as group_permissions \
          \FROM ( \
          \  SELECT project.uuid, project.visibility, project.sharing, project.tenant_uuid \
          \  FROM project \
          \  LEFT JOIN project_perm_group ON project.uuid = project_perm_group.project_uuid AND project.tenant_uuid = project_perm_group.tenant_uuid \
          \  WHERE project_perm_group.user_group_uuid = ? AND project_perm_group.tenant_uuid = ? \
          \) nested_project"
  let params = [toField userGroupUuid, toField tenantUuid]
  logQuery sql params
  let action conn = query conn sql params
  runDB action

findProjectByUuid :: U.UUID -> AppContextM Project
findProjectByUuid projectUuid = do
  tenantUuid <- asks currentTenantUuid
  entity <- createFindEntityByFn entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString projectUuid)]
  enhance entity

findProjectByUuid' :: U.UUID -> AppContextM (Maybe Project)
findProjectByUuid' projectUuid = do
  tenantUuid <- asks currentTenantUuid
  mEntity <- createFindEntityByFn' entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString projectUuid)]
  case mEntity of
    Just entity -> enhance entity >>= return . Just
    Nothing -> return Nothing

findProjectSuggestionByUuid' :: U.UUID -> AppContextM (Maybe ProjectSuggestion)
findProjectSuggestionByUuid' uuid = do
  tenantUuid <- asks currentTenantUuid
  createFindEntityWithFieldsByFn' "uuid, name, description" entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]

findProjectForSquashing :: AppContextM [U.UUID]
findProjectForSquashing = do
  let sql = "SELECT uuid FROM project WHERE squashed = false"
  logInfoI _CMP_DATABASE (trim sql)
  let action conn = query_ conn (fromString sql)
  entities <- runDB action
  return . concat $ entities

findProjectDetail :: U.UUID -> AppContextM ProjectDetail
findProjectDetail uuid = do
  tenantUuid <- asks currentTenantUuid
  let sql =
        fromString $
          f''
            "SELECT project.uuid, \
            \       project.name, \
            \       project.visibility, \
            \       project.sharing, \
            \       project.knowledge_model_package_id, \
            \       project.selected_question_tag_uuids, \
            \       project.is_template, \
            \       project_mig.new_project_uuid AS migration_uuid, \
            \       ${projectDetailPermSql}, \
            \       ( \
            \        WITH pkg AS (SELECT (string_to_array(project.knowledge_model_package_id, ':'))[1] AS org_id, \
            \                            (string_to_array(project.knowledge_model_package_id, ':'))[2] AS km_id, \
            \                            (string_to_array(project.knowledge_model_package_id, ':'))[3] AS ver) \
            \        SELECT count(*) \
            \        FROM project_action \
            \             LEFT JOIN pkg k ON true \
            \        WHERE project_action.metamodel_version = ${projectActionMetamodelVersion} \
            \          AND exists (SELECT 1 \
            \                       FROM jsonb_array_elements(project_action.allowed_packages) AS spec(elem) \
            \                       WHERE ((spec.elem ->> 'kmId') IS NULL OR (spec.elem ->> 'kmId') = k.km_id) \
            \                         AND ((spec.elem ->> 'orgId') IS NULL OR (spec.elem ->> 'orgId') = k.org_id) \
            \                         AND ((spec.elem ->> 'minVersion') IS NULL OR \
            \                              compare_version(k.ver, spec.elem ->> 'minVersion') IN ('GT', 'EQ')) \
            \                         AND ((spec.elem ->> 'maxVersion') IS NULL OR \
            \                              compare_version(k.ver, spec.elem ->> 'maxVersion') IN ('LT', 'EQ')) \
            \            ) \
            \       ) AS project_actions, \
            \       ( \
            \        WITH pkg AS (SELECT (string_to_array(project.knowledge_model_package_id, ':'))[1] AS org_id, \
            \                            (string_to_array(project.knowledge_model_package_id, ':'))[2] AS km_id, \
            \                            (string_to_array(project.knowledge_model_package_id, ':'))[3] AS ver) \
            \        SELECT count(*) \
            \        FROM project_importer \
            \             LEFT JOIN pkg k ON true \
            \        WHERE project_importer.metamodel_version = ${projectImporterMetamodelVersion} \
            \          AND exists (SELECT 1 \
            \                       FROM jsonb_array_elements(project_importer.allowed_packages) AS spec(elem) \
            \                       WHERE ((spec.elem ->> 'kmId') IS NULL OR (spec.elem ->> 'kmId') = k.km_id) \
            \                         AND ((spec.elem ->> 'orgId') IS NULL OR (spec.elem ->> 'orgId') = k.org_id) \
            \                         AND ((spec.elem ->> 'minVersion') IS NULL OR \
            \                              compare_version(k.ver, spec.elem ->> 'minVersion') IN ('GT', 'EQ')) \
            \                         AND ((spec.elem ->> 'maxVersion') IS NULL OR \
            \                              compare_version(k.ver, spec.elem ->> 'maxVersion') IN ('LT', 'EQ')) \
            \            ) \
            \       ) AS project_importers, \
            \       ( \
            \        SELECT count(*) \
            \        FROM project_file \
            \        WHERE tenant_uuid = '${tenantUuid}' AND project_uuid = '${projectUuid}' \
            \       ) as file_count \
            \FROM project \
            \LEFT JOIN project_migration project_mig ON project.uuid = project_mig.old_project_uuid AND project.tenant_uuid = project_mig.tenant_uuid \
            \WHERE project.tenant_uuid = ? AND project.uuid = ?"
            [ ("projectUuid", U.toString uuid)
            , ("projectDetailPermSql", projectDetailPermSql)
            , ("projectActionMetamodelVersion", show projectActionMetamodelVersion)
            , ("projectImporterMetamodelVersion", show projectImporterMetamodelVersion)
            , ("tenantUuid", U.toString tenantUuid)
            ]
  let queryParams = [("tenant_uuid", U.toString tenantUuid), ("uuid", U.toString uuid)]
  let params = fmap snd queryParams
  logQuery sql params
  let action conn = query conn sql params
  runOneEntityDB entityName action queryParams

findProjectDetailQuestionnaire :: U.UUID -> AppContextM ProjectDetailQuestionnaire
findProjectDetailQuestionnaire uuid = do
  tenantUuid <- asks currentTenantUuid
  let sql =
        fromString $
          f''
            "SELECT project.uuid, \
            \       project.name, \
            \       project.visibility, \
            \       project.sharing, \
            \       project.knowledge_model_package_id, \
            \       project.selected_question_tag_uuids, \
            \       project.is_template, \
            \       project_mig.new_project_uuid AS migration_uuid, \
            \       ${projectDetailPermSql}, \
            \       ( \
            \        WITH pkg AS (SELECT (string_to_array(project.knowledge_model_package_id, ':'))[1] AS org_id, \
            \                            (string_to_array(project.knowledge_model_package_id, ':'))[2] AS km_id, \
            \                            (string_to_array(project.knowledge_model_package_id, ':'))[3] AS ver) \
            \        SELECT count(*) \
            \        FROM project_action \
            \             LEFT JOIN pkg k ON true \
            \        WHERE project_action.metamodel_version = ${projectActionMetamodelVersion} \
            \          AND exists (SELECT 1 \
            \                       FROM jsonb_array_elements(project_action.allowed_packages) AS spec(elem) \
            \                       WHERE ((spec.elem ->> 'kmId') IS NULL OR (spec.elem ->> 'kmId') = k.km_id) \
            \                         AND ((spec.elem ->> 'orgId') IS NULL OR (spec.elem ->> 'orgId') = k.org_id) \
            \                         AND ((spec.elem ->> 'minVersion') IS NULL OR \
            \                              compare_version(k.ver, spec.elem ->> 'minVersion') IN ('GT', 'EQ')) \
            \                         AND ((spec.elem ->> 'maxVersion') IS NULL OR \
            \                              compare_version(k.ver, spec.elem ->> 'maxVersion') IN ('LT', 'EQ')) \
            \            ) \
            \       ) AS project_actions, \
            \       ( \
            \        WITH pkg AS (SELECT (string_to_array(project.knowledge_model_package_id, ':'))[1] AS org_id, \
            \                            (string_to_array(project.knowledge_model_package_id, ':'))[2] AS km_id, \
            \                            (string_to_array(project.knowledge_model_package_id, ':'))[3] AS ver) \
            \        SELECT count(*) \
            \        FROM project_importer \
            \             LEFT JOIN pkg k ON true \
            \        WHERE project_importer.metamodel_version = ${projectImporterMetamodelVersion} \
            \          AND exists (SELECT 1 \
            \                       FROM jsonb_array_elements(project_importer.allowed_packages) AS spec(elem) \
            \                       WHERE ((spec.elem ->> 'kmId') IS NULL OR (spec.elem ->> 'kmId') = k.km_id) \
            \                         AND ((spec.elem ->> 'orgId') IS NULL OR (spec.elem ->> 'orgId') = k.org_id) \
            \                         AND ((spec.elem ->> 'minVersion') IS NULL OR \
            \                              compare_version(k.ver, spec.elem ->> 'minVersion') IN ('GT', 'EQ')) \
            \                         AND ((spec.elem ->> 'maxVersion') IS NULL OR \
            \                              compare_version(k.ver, spec.elem ->> 'maxVersion') IN ('LT', 'EQ')) \
            \            ) \
            \       ) AS project_importers, \
            \       ( \
            \        SELECT array_agg(concat(uuid, '<:::::>', \
            \                                file_name, '<:::::>', \
            \                                content_type, '<:::::>', \
            \                                file_size \
            \                        )) \
            \        FROM project_file \
            \        WHERE tenant_uuid = '${tenantUuid}' AND project_uuid = '${projectUuid}' \
            \       ) as files \
            \FROM project \
            \LEFT JOIN project_migration project_mig ON project.uuid = project_mig.old_project_uuid AND project.tenant_uuid = project_mig.tenant_uuid \
            \WHERE project.tenant_uuid = ? AND project.uuid = ?"
            [ ("projectUuid", U.toString uuid)
            , ("projectDetailPermSql", projectDetailPermSql)
            , ("projectActionMetamodelVersion", show projectActionMetamodelVersion)
            , ("projectImporterMetamodelVersion", show projectImporterMetamodelVersion)
            , ("tenantUuid", U.toString tenantUuid)
            ]
  let queryParams = [("tenant_uuid", U.toString tenantUuid), ("uuid", U.toString uuid)]
  let params = fmap snd queryParams
  logQuery sql params
  let action conn = query conn sql params
  runOneEntityDB entityName action queryParams

findProjectDetailPreview :: U.UUID -> AppContextM ProjectDetailPreview
findProjectDetailPreview uuid = do
  tenantUuid <- asks currentTenantUuid
  let sql =
        fromString $
          f''
            "SELECT project.uuid, \
            \       project.name, \
            \       project.visibility, \
            \       project.sharing, \
            \       project.knowledge_model_package_id, \
            \       project.is_template, \
            \       project.document_template_id, \
            \       project_mig.new_project_uuid AS migration_uuid, \
            \       ${projectDetailPermSql}, \
            \       dt_format.uuid, \
            \       dt_format.name, \
            \       dt_format.icon, \
            \       ( \
            \        SELECT count(*) \
            \        FROM project_file \
            \        WHERE tenant_uuid = '${tenantUuid}' AND project_uuid = '${projectUuid}' \
            \       ) as file_count \
            \FROM project \
            \LEFT JOIN project_migration project_mig ON project.uuid = project_mig.old_project_uuid AND project.tenant_uuid = project_mig.tenant_uuid \
            \LEFT JOIN document_template dt ON project.document_template_id = dt.id AND project.tenant_uuid = dt.tenant_uuid \
            \LEFT JOIN document_template_format dt_format ON project.document_template_id = dt_format.document_template_id AND project.format_uuid = dt_format.uuid AND project.tenant_uuid = dt_format.tenant_uuid \
            \WHERE project.tenant_uuid = ? AND project.uuid = ?"
            [ ("projectDetailPermSql", projectDetailPermSql)
            , ("projectUuid", U.toString uuid)
            , ("tenantUuid", U.toString tenantUuid)
            ]
  let queryParams = [("tenant_uuid", U.toString tenantUuid), ("uuid", U.toString uuid)]
  let params = fmap snd queryParams
  logQuery sql params
  let action conn = query conn sql params
  runOneEntityDB entityName action queryParams

findProjectDetailSettings :: U.UUID -> AppContextM ProjectDetailSettings
findProjectDetailSettings uuid = do
  tenantUuid <- asks currentTenantUuid
  let sql =
        fromString $
          f''
            "SELECT project.uuid, \
            \       project.name, \
            \       project.description, \
            \       project.visibility, \
            \       project.sharing, \
            \       project.is_template, \
            \       project.project_tags, \
            \       project.selected_question_tag_uuids, \
            \       project.format_uuid, \
            \       project_mig.new_project_uuid AS migration_uuid, \
            \       ${projectDetailPermSql}, \
            \       pkg.id                         as knowledge_model_package_id, \
            \       pkg.name                       as knowledge_model_package_name, \
            \       pkg.organization_id            as knowledge_model_package_organization_id, \
            \       pkg.km_id                      as knowledge_model_package_km_id, \
            \       pkg.version                    as knowledge_model_package_version, \
            \       pkg.phase                      as knowledge_model_package_phase, \
            \       pkg.description                as knowledge_model_package_description, \
            \       pkg.non_editable               as knowledge_model_package_non_editable, \
            \       pkg.created_at                 as knowledge_model_package_created_at, \
            \       dt.id                          as document_template_id, \
            \       dt.name                        as document_template_name, \
            \       dt.version                     as document_template_version, \
            \       dt.phase                       as document_template_phase, \
            \       dt.description                 as document_template_description, \
            \       ( \
            \        SELECT jsonb_agg(jsonb_build_object('uuid', uuid, 'name', name, 'icon', icon)) \
            \        FROM (SELECT * \
            \              FROM document_template_format dt_format \
            \              WHERE dt_format.tenant_uuid = project.tenant_uuid AND dt_format.document_template_id = dt.id \
            \              ORDER BY dt_format.name) nested \
            \       ) AS document_template_formats, \
            \       dt.metamodel_version           as document_template_metamodel_version, \
            \       ( \
            \        SELECT count(*) \
            \        FROM project_file \
            \        WHERE tenant_uuid = '${tenantUuid}' AND project_uuid = '${projectUuid}' \
            \       ) as file_count \
            \FROM project \
            \LEFT JOIN project_migration project_mig ON project.uuid = project_mig.old_project_uuid AND project.tenant_uuid = project_mig.tenant_uuid \
            \LEFT JOIN knowledge_model_package pkg ON project.knowledge_model_package_id = pkg.id AND project.tenant_uuid = pkg.tenant_uuid \
            \LEFT JOIN document_template dt ON project.document_template_id = dt.id AND project.tenant_uuid = dt.tenant_uuid \
            \WHERE project.tenant_uuid = ? AND project.uuid = ?"
            [ ("projectDetailPermSql", projectDetailPermSql)
            , ("projectUuid", U.toString uuid)
            , ("tenantUuid", U.toString tenantUuid)
            ]
  let queryParams = [("tenant_uuid", U.toString tenantUuid), ("uuid", U.toString uuid)]
  let params = fmap snd queryParams
  logQuery sql params
  let action conn = query conn sql params
  runOneEntityDB entityName action queryParams

projectDetailPermSql :: String
projectDetailPermSql =
  "(SELECT array_agg(CONCAT(project_perm_user.user_uuid, '::', project_perm_user.perms, '::', u.uuid, '::', u.first_name, \
  \                         '::', u.last_name, '::', u.email, '::', u.image_url)) \
  \ FROM project_perm_user \
  \          JOIN user_entity u on u.uuid = project_perm_user.user_uuid \
  \ WHERE project_uuid = project.uuid \
  \ GROUP BY project_uuid)  as user_permissions, \
  \(SELECT array_agg(CONCAT(project_perm_group.user_group_uuid, '::', project_perm_group.perms, '::', ug.uuid, '::', ug.name, \
  \                         '::', ug.private, '::', ug.description)) \
  \ FROM project_perm_group \
  \          JOIN user_group ug on ug.uuid = project_perm_group.user_group_uuid \
  \ WHERE project_uuid = project.uuid \
  \ GROUP BY project_uuid)  as group_permissions"

countProjects :: AppContextM Int
countProjects = do
  tenantUuid <- asks currentTenantUuid
  countProjectsWithTenant tenantUuid

countProjectsWithTenant :: U.UUID -> AppContextM Int
countProjectsWithTenant tenantUuid = createCountByFn entityName tenantCondition [U.toString tenantUuid]

insertProject :: Project -> AppContextM Int64
insertProject project = do
  -- Insert project
  let sql =
        fromString
          "INSERT INTO project VALUES (?, ?, ?, ?, ?, ?::uuid[], ?, ?, ?, ?, ?, ?, ?, ?, ?, ?::text[])"
  let params = toRow project
  logQuery sql params
  let action conn = execute conn sql params
  runDB action
  -- Insert project permissions
  traverse_ insertProjectPerm project.permissions
  return 1

updateProjectByUuid :: Project -> AppContextM ()
updateProjectByUuid project = do
  tenantUuid <- asks currentTenantUuid
  let sql =
        fromString
          "UPDATE project SET uuid = ?, name = ?, visibility = ?, sharing = ?, knowledge_model_package_id = ?, selected_question_tag_uuids = ?::uuid[], document_template_id = ?, format_uuid = ?, created_by = ?, created_at = ?, updated_at = ?, description = ?, is_template = ?, squashed = ?, tenant_uuid = ?, project_tags = ?::text[] WHERE tenant_uuid = ? AND uuid = ?"
  let params = toRow project ++ [toField tenantUuid, toField . U.toText $ project.uuid]
  logInsertAndUpdate sql params
  let action conn = execute conn sql params
  runDB action
  deleteProjectPermsFiltered [("project_uuid", U.toString project.uuid)]
  traverse_ insertProjectPerm project.permissions

updateProjectSquashedByUuid :: U.UUID -> Bool -> AppContextM Int64
updateProjectSquashedByUuid uuid squashed = do
  tenantUuid <- asks currentTenantUuid
  let sql = fromString "UPDATE project SET squashed = ? WHERE tenant_uuid = ? AND uuid = ?"
  let params = [toField squashed, toField tenantUuid, toField . U.toText $ uuid]
  logInsertAndUpdate sql params
  let action conn = execute conn sql params
  runDB action

updateProjectSquashedAndUpdatedAtByUuid :: U.UUID -> Bool -> UTCTime -> AppContextM Int64
updateProjectSquashedAndUpdatedAtByUuid uuid squashed updatedAt = do
  tenantUuid <- asks currentTenantUuid
  let sql = fromString "UPDATE project SET squashed = ?, updated_at = ? WHERE tenant_uuid = ? AND uuid = ?"
  let params = [toField squashed, toField updatedAt, toField tenantUuid, toField . U.toText $ uuid]
  logInsertAndUpdate sql params
  let action conn = execute conn sql params
  runDB action

updateProjectUpdatedAtByUuid :: U.UUID -> AppContextM Int64
updateProjectUpdatedAtByUuid uuid = do
  tenantUuid <- asks currentTenantUuid
  let sql = fromString "UPDATE project SET updated_at = now() WHERE tenant_uuid = ? AND uuid = ?"
  let params = [toField tenantUuid, toField . U.toText $ uuid]
  logInsertAndUpdate sql params
  let action conn = execute conn sql params
  runDB action

deleteProjects :: AppContextM Int64
deleteProjects = createDeleteEntitiesFn entityName

deleteProjectsFiltered :: [(String, String)] -> AppContextM Int64
deleteProjectsFiltered params = do
  tenantUuid <- asks currentTenantUuid
  createDeleteEntitiesByFn entityName (tenantQueryUuid tenantUuid : params)

deleteProjectByUuid :: U.UUID -> AppContextM Int64
deleteProjectByUuid uuid = do
  tenantUuid <- asks currentTenantUuid
  createDeleteEntityByFn entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]

-- ------------------------------------------------------------------------------------------------------------------------------
-- PRIVATE
-- ------------------------------------------------------------------------------------------------------------------------------
projectSelectSql tenantUuid userUuid perm =
  f'
    "SELECT project.* \
    \FROM project \
    \LEFT JOIN project_perm_user ON project.uuid = project_perm_user.project_uuid \
    \LEFT JOIN project_perm_group ON project.uuid = project_perm_group.project_uuid \
    \WHERE %s %s"
    [projectWhereSql tenantUuid userUuid perm]

projectWhereSql tenantUuid userUuid perm =
  f'
    "project.tenant_uuid = '%s' \
    \AND (visibility = 'VisibleEditProjectVisibility' \
    \OR visibility = 'VisibleCommentProjectVisibility' \
    \OR visibility = 'VisibleViewProjectVisibility' \
    \OR (visibility = 'PrivateProjectVisibility' AND project_perm_user.user_uuid = '%s' AND project_perm_user.perms @> ARRAY %s))"
    [tenantUuid, userUuid, perm]

enhance :: Project -> AppContextM Project
enhance project = do
  ps <- findProjectPermsFiltered [("project_uuid", U.toString project.uuid)]
  return $ project {permissions = ps}
