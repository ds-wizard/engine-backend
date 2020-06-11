module Registry.Database.DAO.Package.PackageDAO where

import Data.Bson

import Registry.Database.BSON.Package.Package ()
import Registry.Database.BSON.Package.PackageWithEvents ()
import Registry.Model.Context.AppContext
import Registry.Model.Context.AppContextLenses ()
import Shared.Database.DAO.Common
import Shared.Model.Package.Package
import Shared.Model.Package.PackageWithEvents

entityName = "package"

collection = "packages"

findPackages :: AppContextM [Package]
findPackages = createFindEntitiesFn collection

findPackageWithEvents :: AppContextM [PackageWithEvents]
findPackageWithEvents = createFindEntitiesFn collection

findPackagesFiltered :: [(String, String)] -> AppContextM [Package]
findPackagesFiltered queryParams = createFindEntitiesByFn collection (mapToDBQueryParams queryParams)

findPackagesByKmId :: String -> AppContextM [Package]
findPackagesByKmId kmId = createFindEntitiesByFn collection ["kmId" =: kmId]

findPackagesByOrganizationIdAndKmId :: String -> String -> AppContextM [Package]
findPackagesByOrganizationIdAndKmId organizationId kmId =
  createFindEntitiesByFn collection ["organizationId" =: organizationId, "kmId" =: kmId]

findPackagesByPreviousPackageId :: String -> AppContextM [Package]
findPackagesByPreviousPackageId previousPackageId =
  createFindEntitiesByFn collection ["previousPackageId" =: previousPackageId]

findPackageById :: String -> AppContextM Package
findPackageById = createFindEntityByFn collection entityName "id"

findPackageWithEventsById :: String -> AppContextM PackageWithEvents
findPackageWithEventsById = createFindEntityByFn collection entityName "id"

insertPackage :: PackageWithEvents -> AppContextM Value
insertPackage = createInsertFn collection

deletePackages :: AppContextM ()
deletePackages = createDeleteEntitiesFn collection

deletePackagesFiltered :: [(String, String)] -> AppContextM ()
deletePackagesFiltered queryParams = createDeleteEntitiesByFn collection (mapToDBQueryParams queryParams)

deletePackageById :: String -> AppContextM ()
deletePackageById = createDeleteEntityByFn collection "organizationId"
