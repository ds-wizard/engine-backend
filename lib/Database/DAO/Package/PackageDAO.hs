module Database.DAO.Package.PackageDAO where

import Data.Bson
import Data.Bson.Generic
import Data.Text (Text)
import Database.MongoDB
       ((=:), delete, find, findOne, insert, rest, select)
import Model.Error.Error

import Database.BSON.Package.Package ()
import Database.BSON.Package.PackageWithEvents ()
import Database.DAO.Common
import Model.Context.AppContext
import Model.Package.Package

pkgCollection = "packages"

findPackages :: AppContextM (Either AppError [Package])
findPackages = do
  let action = rest =<< find (select [] pkgCollection)
  packagesS <- runDB action
  return . deserializeEntities $ packagesS

findPackagesFiltered :: [(Text, Text)] -> AppContextM (Either AppError [Package])
findPackagesFiltered queryParams = do
  let filter = (\(p, v) -> p =: v) <$> queryParams
  let action = rest =<< find (select filter pkgCollection)
  packagesS <- runDB action
  return . deserializeEntities $ packagesS

findPackagesByKmId :: String -> AppContextM (Either AppError [Package])
findPackagesByKmId kmId = do
  let action = rest =<< find (select ["kmId" =: kmId] pkgCollection)
  packagesS <- runDB action
  return . deserializeEntities $ packagesS

findPackagesByOrganizationIdAndKmId :: String -> String -> AppContextM (Either AppError [Package])
findPackagesByOrganizationIdAndKmId organizationId kmId = do
  let action = rest =<< find (select ["organizationId" =: organizationId, "kmId" =: kmId] pkgCollection)
  packagesS <- runDB action
  return . deserializeEntities $ packagesS

findPackagesByParentPackageId :: String -> AppContextM (Either AppError [Package])
findPackagesByParentPackageId parentPackageId = do
  let action = rest =<< find (select ["parentPackageId" =: parentPackageId] pkgCollection)
  packagesS <- runDB action
  return . deserializeEntities $ packagesS

findPackageById :: String -> AppContextM (Either AppError Package)
findPackageById pkgId = do
  let action = findOne $ select ["id" =: pkgId] pkgCollection
  maybePackageS <- runDB action
  return . deserializeMaybeEntity $ maybePackageS

findPackageWithEventsById :: String -> AppContextM (Either AppError PackageWithEvents)
findPackageWithEventsById pkgId = do
  let action = findOne $ select ["id" =: pkgId] pkgCollection
  maybePackageS <- runDB action
  return . deserializeMaybeEntity $ maybePackageS

insertPackage :: PackageWithEvents -> AppContextM Value
insertPackage package = do
  let action = insert pkgCollection (toBSON package)
  runDB action

deletePackages :: AppContextM ()
deletePackages = do
  let action = delete $ select [] pkgCollection
  runDB action

deletePackagesFiltered :: [(Text, Text)] -> AppContextM ()
deletePackagesFiltered queryParams = do
  let filter = (\(p, v) -> p =: v) <$> queryParams
  let action = delete $ select filter pkgCollection
  runDB action

deletePackageById :: String -> AppContextM ()
deletePackageById pkgId = do
  let action = delete $ select ["id" =: pkgId] pkgCollection
  runDB action

-- --------------------------------
-- HELPERS
-- --------------------------------
heFindPackages callback = do
  eitherPackages <- findPackages
  case eitherPackages of
    Right packages -> callback packages
    Left error -> return . Left $ error

-- --------------------------------
heFindPackagesFiltered queryParams callback = do
  eitherPackages <- findPackagesFiltered queryParams
  case eitherPackages of
    Right packages -> callback packages
    Left error -> return . Left $ error

-- --------------------------------
heFindPackagesByOrganizationIdAndKmId organizationId kmId callback = do
  eitherPackages <- findPackagesByOrganizationIdAndKmId organizationId kmId
  case eitherPackages of
    Right packages -> callback packages
    Left error -> return . Left $ error

-- --------------------------------
heFindPackageById pkgId callback = do
  eitherPackage <- findPackageById pkgId
  case eitherPackage of
    Right package -> callback package
    Left error -> return . Left $ error

hmFindPackageById pkgId callback = do
  eitherPackage <- findPackageById pkgId
  case eitherPackage of
    Right package -> callback package
    Left error -> return . Just $ error

-- --------------------------------
heFindPackageWithEventsById pkgId callback = do
  eitherPackage <- findPackageWithEventsById pkgId
  case eitherPackage of
    Right package -> callback package
    Left error -> return . Left $ error
