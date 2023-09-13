module Wizard.Database.DAO.Package.PackageDAO where

import Control.Monad.Reader (asks)
import qualified Data.List as L
import Data.Maybe (fromMaybe)
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
import Shared.Common.Util.String (replace)
import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.Package.PackageList ()
import Wizard.Database.Mapping.Package.PackageSuggestion ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.Package.PackageList
import Wizard.Model.Package.PackageSuggestion
import WizardLib.KnowledgeModel.Database.Mapping.Package.PackageWithEvents ()
import WizardLib.KnowledgeModel.Model.Package.Package
import WizardLib.KnowledgeModel.Model.Package.PackageWithEvents

entityName = "package"

pageLabel = "packages"

findPackagesPage
  :: Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> Pageable
  -> [Sort]
  -> AppContextM (Page PackageList)
findPackagesPage mOrganizationId mKmId mQuery mPackageState pageable sort =
  createFindEntitiesGroupByCoordinatePageableQuerySortFn
    entityName
    pageLabel
    pageable
    sort
    "id, package.name, package.organization_id, package.km_id, version, phase, description, non_editable, get_package_state(registry_package.remote_version, version), registry_package.remote_version, registry_organization.name as org_name, registry_organization.logo as org_logo, package.created_at"
    "km_id"
    mQuery
    Nothing
    mOrganizationId
    mKmId
    mPackageState
    ( case mPackageState of
        Just _ -> " AND get_package_state(registry_package.remote_version, version) = ?"
        Nothing -> ""
    )

findPackageSuggestionsPage
  :: Maybe String -> Maybe [String] -> Maybe [String] -> Maybe PackagePhase -> Maybe Bool -> Pageable -> [Sort] -> AppContextM (Page PackageSuggestion)
findPackageSuggestionsPage mQuery mSelectIds mExcludeIds mPhase mNonEditable pageable sort =
  -- 1. Prepare variables
  do
    appUuid <- asks currentAppUuid
    let (sizeI, pageI, skip, limit) = preparePaginationVariables pageable
    let mSelectIdsLike = fmap (fmap (replace "all" "%")) mSelectIds
    let selectCondition =
          case mSelectIds of
            Nothing -> ""
            Just [] -> ""
            Just selectIds ->
              let mapFn _ = " id LIKE ?"
               in " AND (" ++ L.intercalate " OR " (fmap mapFn selectIds) ++ ")"
    let mExcludeIdsLike = fmap (fmap (replace "all" "%")) mExcludeIds
    let excludeCondition =
          case mExcludeIds of
            Nothing -> ""
            Just [] -> ""
            Just excludeIds ->
              let mapFn _ = " id NOT LIKE ?"
               in " AND (" ++ L.intercalate " AND " (fmap mapFn excludeIds) ++ ")"
    let phaseCondition =
          case mPhase of
            Just phase -> f' "AND phase = '%s'" [show phase]
            Nothing -> ""
    let nonEditableCondition =
          case mNonEditable of
            Just nonEditable -> f' "AND non_editable = '%s'" [show nonEditable]
            Nothing -> ""
    -- 2. Get total count
    count <- countPackageSuggestions mQuery selectCondition excludeCondition mSelectIdsLike mExcludeIdsLike phaseCondition nonEditableCondition
    -- 3. Get entities
    let sql =
          fromString $
            f'
              "SELECT id, \
              \        name, \
              \        version, \
              \        description \
              \FROM package outer_package \
              \WHERE app_uuid = ? AND id IN ( \
              \    SELECT CONCAT(organization_id, ':', km_id, ':', \
              \                  (max(string_to_array(version, '.')::int[]))[1] || '.' || \
              \                  (max(string_to_array(version, '.')::int[]))[2] || '.' || \
              \                  (max(string_to_array(version, '.')::int[]))[3]) \
              \    FROM package \
              \    WHERE app_uuid = ? \
              \      AND (name ~* ? OR id ~* ?) %s %s %s %s \
              \    GROUP BY organization_id, km_id) \
              \%s \
              \OFFSET %s \
              \LIMIT %s"
              [selectCondition, excludeCondition, phaseCondition, nonEditableCondition, mapSort sort, show skip, show sizeI]
    let params =
          [U.toString appUuid, U.toString appUuid]
            ++ [regex mQuery]
            ++ [regex mQuery]
            ++ fromMaybe [] mSelectIdsLike
            ++ fromMaybe [] mExcludeIdsLike
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

findSeriesOfPackagesRecursiveById :: String -> AppContextM [PackageWithEvents]
findSeriesOfPackagesRecursiveById pkgId = do
  appUuid <- asks currentAppUuid
  let sql =
        fromString
          "WITH RECURSIVE recursive AS ( \
          \  SELECT *, 1 as level \
          \  FROM package \
          \  WHERE app_uuid = ? AND id = ? \
          \  UNION ALL \
          \  SELECT e.*, level + 1 as level \
          \  FROM package e \
          \  INNER JOIN recursive r ON e.id = r.previous_package_id AND e.app_uuid = ? \
          \) \
          \SELECT id, name, organization_id, km_id, version, metamodel_version, description, readme, license, previous_package_id, fork_of_package_id, merge_checkpoint_package_id, events, created_at, app_uuid, phase, non_editable \
          \FROM recursive \
          \ORDER BY level DESC;"
  let params = [U.toString appUuid, pkgId, U.toString appUuid]
  logQuery sql params
  let action conn = query conn sql params
  runDB action

countPackageSuggestions :: Maybe String -> String -> String -> Maybe [String] -> Maybe [String] -> String -> String -> AppContextM Int
countPackageSuggestions mQuery selectCondition excludeCondition mSelectIdsLike mExcludeIdsLike phaseCondition nonEditableCondition = do
  appUuid <- asks currentAppUuid
  let sql =
        fromString $
          f'
            "SELECT COUNT(*) \
            \FROM (SELECT COUNT(*) \
            \   FROM package \
            \   WHERE app_uuid = ? AND (name ~* ? OR id ~* ?) %s %s %s %s \
            \   GROUP BY organization_id, km_id) nested"
            [selectCondition, excludeCondition, phaseCondition, nonEditableCondition]
  let params =
        [U.toString appUuid, regex mQuery, regex mQuery]
          ++ fromMaybe [] mSelectIdsLike
          ++ fromMaybe [] mExcludeIdsLike
  logQuery sql params
  let action conn = query conn sql params
  result <- runDB action
  case result of
    [count] -> return . fromOnly $ count
    _ -> return 0

updatePackagePhaseById :: String -> PackagePhase -> AppContextM Int64
updatePackagePhaseById pkgId phase = do
  appUuid <- asks currentAppUuid
  let sql = fromString "UPDATE package SET phase = ? WHERE app_uuid = ? AND id = ?"
  let params = [toField phase, toField appUuid, toField pkgId]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action
