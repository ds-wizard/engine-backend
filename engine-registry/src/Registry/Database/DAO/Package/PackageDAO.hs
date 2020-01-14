module Registry.Database.DAO.Package.PackageDAO where

import Data.Bson
import Data.Text (Text)
import Database.MongoDB ((=:))
import Shared.Model.Error.Error

import Registry.Database.BSON.Package.Package ()
import Registry.Database.BSON.Package.PackageWithEvents ()
import Registry.Database.DAO.Common
import Registry.Model.Context.AppContext
import Shared.Model.Package.Package
import Shared.Model.Package.PackageWithEvents
import Shared.Util.Helper (createHeeHelper, createHemHelper)

entityName = "package"

collection = "packages"

findPackages :: AppContextM (Either AppError [Package])
findPackages = createFindEntitiesFn collection

findPackageWithEvents :: AppContextM (Either AppError [PackageWithEvents])
findPackageWithEvents = createFindEntitiesFn collection

findPackagesFiltered :: [(Text, Text)] -> AppContextM (Either AppError [Package])
findPackagesFiltered queryParams = createFindEntitiesByFn collection (mapToDBQueryParams queryParams)

findPackagesByKmId :: String -> AppContextM (Either AppError [Package])
findPackagesByKmId kmId = createFindEntitiesByFn collection ["kmId" =: kmId]

findPackagesByOrganizationIdAndKmId :: String -> String -> AppContextM (Either AppError [Package])
findPackagesByOrganizationIdAndKmId organizationId kmId =
  createFindEntitiesByFn collection ["organizationId" =: organizationId, "kmId" =: kmId]

findPackagesByPreviousPackageId :: String -> AppContextM (Either AppError [Package])
findPackagesByPreviousPackageId previousPackageId =
  createFindEntitiesByFn collection ["previousPackageId" =: previousPackageId]

findPackageById :: String -> AppContextM (Either AppError Package)
findPackageById = createFindEntityByFn collection entityName "id"

findPackageWithEventsById :: String -> AppContextM (Either AppError PackageWithEvents)
findPackageWithEventsById = createFindEntityByFn collection entityName "id"

insertPackage :: PackageWithEvents -> AppContextM Value
insertPackage = createInsertFn collection

deletePackages :: AppContextM ()
deletePackages = createDeleteEntitiesFn collection

deletePackagesFiltered :: [(Text, Text)] -> AppContextM ()
deletePackagesFiltered queryParams = createDeleteEntitiesByFn collection (mapToDBQueryParams queryParams)

deletePackageById :: String -> AppContextM ()
deletePackageById = createDeleteEntityByFn collection "organizationId"

-- --------------------------------
-- HELPERS
-- --------------------------------
heFindPackages callback = createHeeHelper findPackages callback

-- --------------------------------
heFindPackagesFiltered queryParams callback = createHeeHelper (findPackagesFiltered queryParams) callback

hmFindPackagesFiltered queryParams callback = createHemHelper (findPackagesFiltered queryParams) callback

-- --------------------------------
heFindPackagesByOrganizationIdAndKmId organizationId kmId callback =
  createHeeHelper (findPackagesByOrganizationIdAndKmId organizationId kmId) callback

-- --------------------------------
heFindPackageById pkgId callback = createHeeHelper (findPackageById pkgId) callback

hmFindPackageById pkgId callback = createHemHelper (findPackageById pkgId) callback

-- --------------------------------
heFindPackageWithEventsById pkgId callback = createHeeHelper (findPackageWithEventsById pkgId) callback
