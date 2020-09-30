module Shared.Database.DAO.Package.PackageDAO where

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader)
import Data.Bson

import Shared.Database.BSON.Package.Package ()
import Shared.Database.BSON.Package.PackageGroup ()
import Shared.Database.BSON.Package.PackageWithEvents ()
import Shared.Database.DAO.Common
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Shared.Model.Context.ContextLenses
import Shared.Model.Error.Error
import Shared.Model.Package.Package
import Shared.Model.Package.PackageGroup
import Shared.Model.Package.PackageWithEvents

entityName = "package"

collection = "packages"

findPackages :: (MonadError AppError m, MonadReader s m, HasPool' s, MonadIO m) => m [Package]
findPackages = createFindEntitiesFn collection

findPackageWithEvents :: (MonadError AppError m, MonadReader s m, HasPool' s, MonadIO m) => m [PackageWithEvents]
findPackageWithEvents = createFindEntitiesFn collection

findPackagesFiltered ::
     (MonadError AppError m, MonadReader s m, HasPool' s, MonadIO m) => [(String, String)] -> m [Package]
findPackagesFiltered queryParams = createFindEntitiesByFn collection (mapToDBQueryParams queryParams)

findPackageGroups ::
     (MonadError AppError m, MonadReader s m, HasPool' s, MonadIO m)
  => Maybe String
  -> Maybe String
  -> Maybe String
  -> Pageable
  -> [Sort]
  -> m (Page PackageGroup)
findPackageGroups mOrganizationId mKmId mQuery pageable sort =
  createAggregateEntitiesPageableQuerySortFn
    collection
    pageable
    sort
    [ "$group" =:
      [ "_id" =: ["organizationId" =: "$organizationId", "kmId" =: "$kmId"]
      , "organizationId" =: ["$first" =: "$organizationId"]
      , "kmId" =: ["$first" =: "$kmId"]
      , "versions" =:
        [ "$addToSet" =:
          [ "id" =: "$id"
          , "name" =: "$name"
          , "organizationId" =: "$organizationId"
          , "kmId" =: "$kmId"
          , "version" =: "$version"
          , "metamodelVersion" =: "$metamodelVersion"
          , "description" =: "$description"
          , "readme" =: "$readme"
          , "license" =: "$license"
          , "previousPackageId" =: "$previousPackageId"
          , "forkOfPackageId" =: "$forkOfPackageId"
          , "mergeCheckpointPackageId" =: "$mergeCheckpointPackageId"
          , "createdAt" =: "$createdAt"
          ]
        ]
      ]
    ] =<<
  sel [regexSel "versions.name" mQuery, textMaybeSel "organizationId" mOrganizationId, textMaybeSel "kmId" mKmId]

findPackagesByOrganizationIdAndKmId ::
     (MonadError AppError m, MonadReader s m, HasPool' s, MonadIO m) => String -> String -> m [Package]
findPackagesByOrganizationIdAndKmId organizationId kmId =
  createFindEntitiesByFn collection ["organizationId" =: organizationId, "kmId" =: kmId]

findPackagesByPreviousPackageId ::
     (MonadError AppError m, MonadReader s m, HasPool' s, MonadIO m) => String -> m [Package]
findPackagesByPreviousPackageId previousPackageId =
  createFindEntitiesByFn collection ["previousPackageId" =: previousPackageId]

findPackagesByForkOfPackageId ::
     (MonadError AppError m, MonadReader s m, HasPool' s, MonadIO m) => String -> m [Package]
findPackagesByForkOfPackageId forkOfPackageId = createFindEntitiesByFn collection ["forkOfPackageId" =: forkOfPackageId]

findPackageById :: (MonadError AppError m, MonadReader s m, HasPool' s, MonadIO m) => String -> m Package
findPackageById = createFindEntityByFn collection entityName "id"

findPackageById' :: (MonadError AppError m, MonadReader s m, HasPool' s, MonadIO m) => String -> m (Maybe Package)
findPackageById' = createFindEntityByFn' collection entityName "id"

findPackageWithEventsById ::
     (MonadError AppError m, MonadReader s m, HasPool' s, MonadIO m) => String -> m PackageWithEvents
findPackageWithEventsById = createFindEntityByFn collection entityName "id"

countPackages :: (MonadError AppError m, MonadReader s m, HasPool' s, MonadIO m) => m Int
countPackages = createCountFn collection

insertPackage :: (MonadError AppError m, MonadReader s m, HasPool' s, MonadIO m) => PackageWithEvents -> m Value
insertPackage = createInsertFn collection

deletePackages :: (MonadError AppError m, MonadReader s m, HasPool' s, MonadIO m) => m ()
deletePackages = createDeleteEntitiesFn collection

deletePackagesFiltered :: (MonadError AppError m, MonadReader s m, HasPool' s, MonadIO m) => [(String, String)] -> m ()
deletePackagesFiltered queryParams = createDeleteEntitiesByFn collection (mapToDBQueryParams queryParams)

deletePackageById :: (MonadError AppError m, MonadReader s m, HasPool' s, MonadIO m) => String -> m ()
deletePackageById = createDeleteEntityByFn collection "id"
