module Wizard.Database.DAO.Package.PackageDAO where

import Data.Bson
import Data.Text (Text)
import Database.MongoDB ((=:))

import Shared.Model.Error.Error
import Shared.Model.Package.Package
import Shared.Model.Package.PackageWithEvents
import Shared.Util.Helper (createHeeHelper, createHemHelper)
import Wizard.Database.BSON.Package.Package ()
import Wizard.Database.BSON.Package.PackageWithEvents ()
import Wizard.Database.DAO.Common
import Wizard.Model.Context.AppContext

entityName = "package"

collection = "packages"

findPackages :: AppContextM (Either AppError [Package])
findPackages = createFindEntitiesFn collection

findPackageWithEvents :: AppContextM (Either AppError [PackageWithEvents])
findPackageWithEvents = createFindEntitiesFn collection

findPackagesFiltered :: [(Text, Text)] -> AppContextM (Either AppError [Package])
findPackagesFiltered queryParams = createFindEntitiesByFn collection (mapToDBQueryParams queryParams)

findPackagesByOrganizationIdAndKmId :: String -> String -> AppContextM (Either AppError [Package])
findPackagesByOrganizationIdAndKmId organizationId kmId =
  createFindEntitiesByFn collection ["organizationId" =: organizationId, "kmId" =: kmId]

findPackagesByPreviousPackageId :: String -> AppContextM (Either AppError [Package])
findPackagesByPreviousPackageId previousPackageId =
  createFindEntitiesByFn collection ["previousPackageId" =: previousPackageId]

findPackagesByForkOfPackageId :: String -> AppContextM (Either AppError [Package])
findPackagesByForkOfPackageId forkOfPackageId = createFindEntitiesByFn collection ["forkOfPackageId" =: forkOfPackageId]

findPackageById :: String -> AppContextM (Either AppError Package)
findPackageById = createFindEntityByFn collection entityName "id"

findPackageWithEventsById :: String -> AppContextM (Either AppError PackageWithEvents)
findPackageWithEventsById = createFindEntityByFn collection entityName "id"

countPackages :: AppContextM (Either AppError Int)
countPackages = createCountFn collection

insertPackage :: PackageWithEvents -> AppContextM Value
insertPackage = createInsertFn collection

deletePackages :: AppContextM ()
deletePackages = createDeleteEntitiesFn collection

deletePackagesFiltered :: [(Text, Text)] -> AppContextM ()
deletePackagesFiltered queryParams = createDeleteEntitiesByFn collection (mapToDBQueryParams queryParams)

deletePackageById :: String -> AppContextM ()
deletePackageById = createDeleteEntityByFn collection "id"

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

-- -----------------------------------------------------
heCountPackages callback = createHeeHelper countPackages callback
