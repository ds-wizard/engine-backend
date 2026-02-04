module Wizard.Database.DAO.KnowledgeModel.KnowledgeModelPackageDAO where

import Control.Monad.Reader (asks)
import qualified Data.List as L
import Data.String (fromString)
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import GHC.Int

import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.PageMetadata
import Shared.Common.Model.Common.Pageable
import Shared.Common.Model.Common.Sort
import Shared.Common.Util.Logger
import Shared.Coordinate.Model.Coordinate.Coordinate
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.KnowledgeModel.Package.KnowledgeModelPackageList ()
import Wizard.Database.Mapping.KnowledgeModel.Package.KnowledgeModelPackageSuggestion ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.KnowledgeModel.Package.KnowledgeModelPackageList
import Wizard.Model.KnowledgeModel.Package.KnowledgeModelPackageSuggestion

entityName = "knowledge_model_package"

pageLabel = "knowledgeModelPackages"

findPackagesPage
  :: Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe Bool
  -> Pageable
  -> [Sort]
  -> AppContextM (Page KnowledgeModelPackageList)
findPackagesPage mOrganizationId mKmId mQuery mOutdated pageable sort =
  createFindEntitiesGroupByCoordinatePageableQuerySortFn
    entityName
    pageLabel
    pageable
    sort
    "uuid, knowledge_model_package.name, knowledge_model_package.organization_id, knowledge_model_package.km_id, version, phase, description, non_editable, public, registry_knowledge_model_package.remote_version, registry_organization.name as org_name, registry_organization.logo as org_logo, knowledge_model_package.created_at"
    "km_id"
    mQuery
    Nothing
    mOrganizationId
    mKmId
    mOutdated
    ( case mOutdated of
        Just _ -> " AND is_outdated(registry_knowledge_model_package.remote_version, version) = ?"
        Nothing -> ""
    )

findPackageSuggestionByUuid :: U.UUID -> AppContextM KnowledgeModelPackageSuggestion
findPackageSuggestionByUuid uuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityWithFieldsByFn "uuid, name, organization_id, km_id, version, description" False entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]

findPackageSuggestionsPage :: Maybe String -> Maybe [Coordinate] -> Maybe [Coordinate] -> Maybe KnowledgeModelPackagePhase -> Maybe Bool -> Pageable -> [Sort] -> AppContextM (Page KnowledgeModelPackageSuggestion)
findPackageSuggestionsPage mQuery mSelectCoordinates mExcludeCoordinates mPhase mNonEditable pageable sort =
  -- 1. Prepare variables
  do
    tenantUuid <- asks currentTenantUuid
    let (sizeI, pageI, skip, limit) = preparePaginationVariables pageable
    let (selectCondition, selectParams) =
          case mSelectCoordinates of
            Nothing -> ("", [])
            Just [] -> ("", [])
            Just selectCoordinates ->
              let mapFn _ = " (organization_id = ? AND km_id = ?)"
               in (" AND (" ++ L.intercalate " OR " (fmap mapFn selectCoordinates) ++ ")", concatMap (\c -> [c.organizationId, c.entityId]) selectCoordinates)
    let (excludeCondition, excludeParams) =
          case mExcludeCoordinates of
            Nothing -> ("", [])
            Just [] -> ("", [])
            Just excludeCoordinates ->
              let mapFn _ = " NOT(organization_id = ? AND km_id = ?)"
               in (" AND (" ++ L.intercalate " AND " (fmap mapFn excludeCoordinates) ++ ")", concatMap (\c -> [c.organizationId, c.entityId]) excludeCoordinates)
    let phaseCondition =
          case mPhase of
            Just phase -> f' "AND phase = '%s'" [show phase]
            Nothing -> ""
    let nonEditableCondition =
          case mNonEditable of
            Just nonEditable -> f' "AND non_editable = '%s'" [show nonEditable]
            Nothing -> ""
    -- 2. Get total count
    count <- countPackageSuggestions mQuery selectCondition excludeCondition selectParams excludeParams phaseCondition nonEditableCondition
    -- 3. Get entities
    let sql =
          fromString $
            f'
              "SELECT uuid, \
              \       name, \
              \       organization_id, \
              \       km_id, \
              \       version, \
              \       description \
              \FROM knowledge_model_package outer_package \
              \WHERE tenant_uuid = ? AND concat(organization_id, ':', km_id, ':', version) IN ( \
              \    SELECT CONCAT(organization_id, ':', km_id, ':', \
              \                  (max(string_to_array(version, '.')::int[]))[1] || '.' || \
              \                  (max(string_to_array(version, '.')::int[]))[2] || '.' || \
              \                  (max(string_to_array(version, '.')::int[]))[3]) \
              \    FROM knowledge_model_package \
              \    WHERE tenant_uuid = ? \
              \      AND (name ~* ? OR organization_id ~* ? OR km_id ~* ? OR version ~* ?) %s %s %s %s \
              \    GROUP BY organization_id, km_id) \
              \%s \
              \OFFSET %s \
              \LIMIT %s"
              [selectCondition, excludeCondition, phaseCondition, nonEditableCondition, mapSort sort, show skip, show sizeI]
    let params =
          [U.toString tenantUuid, U.toString tenantUuid]
            ++ [regexM mQuery]
            ++ [regexM mQuery]
            ++ [regexM mQuery]
            ++ [regexM mQuery]
            ++ selectParams
            ++ excludeParams
    logQuery sql params
    let action conn = query conn sql params
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

countPackageSuggestions :: Maybe String -> String -> String -> [String] -> [String] -> String -> String -> AppContextM Int
countPackageSuggestions mQuery selectCondition excludeCondition selectParams excludeParams phaseCondition nonEditableCondition = do
  tenantUuid <- asks currentTenantUuid
  let sql =
        fromString $
          f'
            "SELECT COUNT(*) \
            \FROM (SELECT COUNT(*) \
            \   FROM knowledge_model_package \
            \   WHERE tenant_uuid = ? AND (name ~* ? OR organization_id ~* ? OR km_id ~* ? OR version ~* ?) %s %s %s %s \
            \   GROUP BY organization_id, km_id) nested"
            [selectCondition, excludeCondition, phaseCondition, nonEditableCondition]
  let params =
        [U.toString tenantUuid, regexM mQuery, regexM mQuery, regexM mQuery, regexM mQuery]
          ++ selectParams
          ++ excludeParams
  logQuery sql params
  let action conn = query conn sql params
  result <- runDB action
  case result of
    [count] -> return . fromOnly $ count
    _ -> return 0

updatePackageMetamodelVersion :: U.UUID -> Int -> AppContextM Int64
updatePackageMetamodelVersion pkgUuid metamodelVersion = do
  tenantUuid <- asks currentTenantUuid
  let sql = fromString "UPDATE knowledge_model_package SET metamodel_version = ? WHERE tenant_uuid = ? AND uuid = ?"
  let params = [toField metamodelVersion, toField tenantUuid, toField pkgUuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

updatePackagePhaseAndPublicByUuid :: U.UUID -> KnowledgeModelPackagePhase -> Bool -> AppContextM Int64
updatePackagePhaseAndPublicByUuid pkgUuid phase public = do
  tenantUuid <- asks currentTenantUuid
  let sql = fromString "UPDATE knowledge_model_package SET phase = ?, public = ? WHERE tenant_uuid = ? AND uuid = ?"
  let params = [toField phase, toField public, toField tenantUuid, toField pkgUuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action
