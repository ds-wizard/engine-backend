module Shared.Database.DAO.Package.PackageDAO where

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Reader (MonadReader)
import Data.String (fromString)
import Database.PostgreSQL.Simple
import GHC.Int

import Shared.Database.DAO.Common
import Shared.Database.Mapping.Package.Package ()
import Shared.Database.Mapping.Package.PackageWithEvents ()
import Shared.Database.Mapping.Package.PackageWithEventsRaw ()
import Shared.Model.Context.ContextLenses
import Shared.Model.Error.Error
import Shared.Model.Package.Package
import Shared.Model.Package.PackageWithEvents
import Shared.Model.Package.PackageWithEventsRaw
import Shared.Util.Logger

entityName = "package"

findPackages :: (MonadLogger m, MonadError AppError m, MonadReader s m, HasDbPool' s, MonadIO m) => m [Package]
findPackages = createFindEntitiesFn entityName

findPackageWithEvents ::
     (MonadLogger m, MonadError AppError m, MonadReader s m, HasDbPool' s, MonadIO m) => m [PackageWithEvents]
findPackageWithEvents = createFindEntitiesFn entityName

findPackageWithEventsRawById ::
     (MonadLogger m, MonadError AppError m, MonadReader s m, HasDbPool' s, MonadIO m)
  => String
  -> m PackageWithEventsRaw
findPackageWithEventsRawById = createFindEntityByFn entityName "id"

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

findVersionsForPackage ::
     (MonadLogger m, MonadError AppError m, MonadReader s m, HasDbPool' s, MonadIO m) => String -> String -> m [String]
findVersionsForPackage orgId kmId = do
  let sql = "SELECT version FROM package WHERE organization_id = ? and km_id = ?"
  logInfo _CMP_DATABASE sql
  let action conn = query conn (fromString sql) [orgId, kmId]
  versions <- runDB action
  return . fmap fromOnly $ versions

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
