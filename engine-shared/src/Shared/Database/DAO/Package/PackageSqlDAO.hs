module Shared.Database.DAO.Package.PackageSqlDAO where

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Reader (MonadReader)
import GHC.Int

import Shared.Database.BSON.Package.Package ()
import Shared.Database.BSON.Package.PackageGroup ()
import Shared.Database.BSON.Package.PackageWithEvents ()
import Shared.Database.DAO.CommonSql
import Shared.Database.Mapping.Package.Package ()
import Shared.Database.Mapping.Package.PackageWithEvents ()
import Shared.Model.Context.ContextLenses
import Shared.Model.Error.Error
import Shared.Model.Package.Package
import Shared.Model.Package.PackageWithEvents

entityName = "package"

findPackages :: (MonadLogger m, MonadError AppError m, MonadReader s m, HasDbPool' s, MonadIO m) => m [Package]
findPackages = createFindEntitiesFn entityName

findPackageWithEvents ::
     (MonadLogger m, MonadError AppError m, MonadReader s m, HasDbPool' s, MonadIO m) => m [PackageWithEvents]
findPackageWithEvents = createFindEntitiesFn entityName

findPackagesFiltered ::
     (MonadLogger m, MonadError AppError m, MonadReader s m, HasDbPool' s, MonadIO m)
  => [(String, String)]
  -> m [Package]
findPackagesFiltered = createFindEntitiesByFn entityName

findPackagesByOrganizationIdAndKmId ::
     (MonadLogger m, MonadError AppError m, MonadReader s m, HasDbPool' s, MonadIO m) => String -> String -> m [Package]
findPackagesByOrganizationIdAndKmId organizationId kmId =
  createFindEntitiesByFn entityName [("organization_id", organizationId), ("km_id", kmId)]

findPackagesByPreviousPackageId ::
     (MonadLogger m, MonadError AppError m, MonadReader s m, HasDbPool' s, MonadIO m) => String -> m [Package]
findPackagesByPreviousPackageId previousPackageId =
  createFindEntitiesByFn entityName [("previous_package_id", previousPackageId)]

findPackagesByForkOfPackageId ::
     (MonadLogger m, MonadError AppError m, MonadReader s m, HasDbPool' s, MonadIO m) => String -> m [Package]
findPackagesByForkOfPackageId forkOfPackageId =
  createFindEntitiesByFn entityName [("fork_of_package_id", forkOfPackageId)]

findPackageById ::
     (MonadLogger m, MonadError AppError m, MonadReader s m, HasDbPool' s, MonadIO m) => String -> m Package
findPackageById = createFindEntityByFn entityName "id"

findPackageById' ::
     (MonadLogger m, MonadError AppError m, MonadReader s m, HasDbPool' s, MonadIO m) => String -> m (Maybe Package)
findPackageById' = createFindEntityByFn' entityName "id"

findPackageWithEventsById ::
     (MonadLogger m, MonadError AppError m, MonadReader s m, HasDbPool' s, MonadIO m) => String -> m PackageWithEvents
findPackageWithEventsById = createFindEntityByFn entityName "id"

countPackages :: (MonadLogger m, MonadError AppError m, MonadReader s m, HasDbPool' s, MonadIO m) => m Int
countPackages = createCountFn entityName

insertPackage ::
     (MonadLogger m, MonadError AppError m, MonadReader s m, HasDbPool' s, MonadIO m) => PackageWithEvents -> m Int64
insertPackage = createInsertFn entityName

deletePackages :: (MonadLogger m, MonadError AppError m, MonadReader s m, HasDbPool' s, MonadIO m) => m Int64
deletePackages = createDeleteEntitiesFn entityName

deletePackagesFiltered ::
     (MonadLogger m, MonadError AppError m, MonadReader s m, HasDbPool' s, MonadIO m) => [(String, String)] -> m Int64
deletePackagesFiltered = createDeleteEntitiesByFn entityName

deletePackageById ::
     (MonadLogger m, MonadError AppError m, MonadReader s m, HasDbPool' s, MonadIO m) => String -> m Int64
deletePackageById = createDeleteEntityByFn entityName "id"
