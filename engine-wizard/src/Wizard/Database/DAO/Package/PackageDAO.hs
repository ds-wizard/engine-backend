module Wizard.Database.DAO.Package.PackageDAO where

import Data.Bson

import Shared.Database.DAO.Common
import Shared.Model.Package.Package
import Shared.Model.Package.PackageWithEvents
import Wizard.Database.BSON.Package.Package ()
import Wizard.Database.BSON.Package.PackageWithEvents ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.AppContextLenses ()

entityName = "package"

collection = "packages"

findPackages :: AppContextM [Package]
findPackages = createFindEntitiesFn collection

findPackageWithEvents :: AppContextM [PackageWithEvents]
findPackageWithEvents = createFindEntitiesFn collection

findPackagesFiltered :: [(String, String)] -> AppContextM [Package]
findPackagesFiltered queryParams = createFindEntitiesByFn collection (mapToDBQueryParams queryParams)

findPackagesByOrganizationIdAndKmId :: String -> String -> AppContextM [Package]
findPackagesByOrganizationIdAndKmId organizationId kmId =
  createFindEntitiesByFn collection ["organizationId" =: organizationId, "kmId" =: kmId]

findPackagesByPreviousPackageId :: String -> AppContextM [Package]
findPackagesByPreviousPackageId previousPackageId =
  createFindEntitiesByFn collection ["previousPackageId" =: previousPackageId]

findPackagesByForkOfPackageId :: String -> AppContextM [Package]
findPackagesByForkOfPackageId forkOfPackageId = createFindEntitiesByFn collection ["forkOfPackageId" =: forkOfPackageId]

findPackageById :: String -> AppContextM Package
findPackageById = createFindEntityByFn collection entityName "id"

findPackageById' :: String -> AppContextM (Maybe Package)
findPackageById' = createFindEntityByFn' collection entityName "id"

findPackageWithEventsById :: String -> AppContextM PackageWithEvents
findPackageWithEventsById = createFindEntityByFn collection entityName "id"

countPackages :: AppContextM Int
countPackages = createCountFn collection

insertPackage :: PackageWithEvents -> AppContextM Value
insertPackage = createInsertFn collection

deletePackages :: AppContextM ()
deletePackages = createDeleteEntitiesFn collection

deletePackagesFiltered :: [(String, String)] -> AppContextM ()
deletePackagesFiltered queryParams = createDeleteEntitiesByFn collection (mapToDBQueryParams queryParams)

deletePackageById :: String -> AppContextM ()
deletePackageById = createDeleteEntityByFn collection "id"
