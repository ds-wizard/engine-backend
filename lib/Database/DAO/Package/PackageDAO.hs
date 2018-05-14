module Database.DAO.Package.PackageDAO where

import Common.Error
import Control.Lens ((^.))
import Data.Bson
import Data.Bson.Generic
import Data.Text (Text)
import Database.MongoDB
       ((=:), delete, find, findOne, insert, rest, select)
import Database.Persist.MongoDB (runMongoDBPoolDef)

import Common.Context
import Database.BSON.Package.Package ()
import Database.BSON.Package.PackageWithEvents ()
import Database.DAO.Common
import Model.Package.Package

pkgCollection = "packages"

findPackages :: Context -> IO (Either AppError [Package])
findPackages context = do
  let action = rest =<< find (select [] pkgCollection)
  packagesS <- runMongoDBPoolDef action (context ^. ctxDbPool)
  return . deserializeEntities $ packagesS

findPackagesFiltered :: Context -> [(Text, Text)] -> IO (Either AppError [Package])
findPackagesFiltered context queryParams = do
  let filter = (\(p, v) -> p =: v) <$> queryParams
  let action = rest =<< find (select filter pkgCollection)
  packagesS <- runMongoDBPoolDef action (context ^. ctxDbPool)
  return . deserializeEntities $ packagesS

findPackageById :: Context -> String -> IO (Either AppError Package)
findPackageById context pkgId = do
  let action = findOne $ select ["id" =: pkgId] pkgCollection
  maybePackageS <- runMongoDBPoolDef action (context ^. ctxDbPool)
  return . deserializeMaybeEntity $ maybePackageS

findPackagesByKmId :: Context -> String -> IO (Either AppError [Package])
findPackagesByKmId context kmId = do
  let action = rest =<< find (select ["kmId" =: kmId] pkgCollection)
  packagesS <- runMongoDBPoolDef action (context ^. ctxDbPool)
  return . deserializeEntities $ packagesS

findPackageByOrganizationIdAndKmId :: Context -> String -> String -> IO (Either AppError [Package])
findPackageByOrganizationIdAndKmId context organizationId kmId = do
  let action = rest =<< find (select ["organizationId" =: organizationId, "kmId" =: kmId] pkgCollection)
  packagesS <- runMongoDBPoolDef action (context ^. ctxDbPool)
  return . deserializeEntities $ packagesS

findPackagesByParentPackageId :: Context -> String -> IO (Either AppError [Package])
findPackagesByParentPackageId context parentPackageId = do
  let action = rest =<< find (select ["parentPackageId" =: parentPackageId] pkgCollection)
  packagesS <- runMongoDBPoolDef action (context ^. ctxDbPool)
  return . deserializeEntities $ packagesS

findPackageWithEventsById :: Context -> String -> IO (Either AppError PackageWithEvents)
findPackageWithEventsById context pkgId = do
  let action = findOne $ select ["id" =: pkgId] pkgCollection
  maybePackageS <- runMongoDBPoolDef action (context ^. ctxDbPool)
  return . deserializeMaybeEntity $ maybePackageS

insertPackage :: Context -> PackageWithEvents -> IO Value
insertPackage context package = do
  let action = insert pkgCollection (toBSON package)
  runMongoDBPoolDef action (context ^. ctxDbPool)

deletePackages :: Context -> IO ()
deletePackages context = do
  let action = delete $ select [] pkgCollection
  runMongoDBPoolDef action (context ^. ctxDbPool)

deletePackagesFiltered :: Context -> [(Text, Text)] -> IO ()
deletePackagesFiltered context queryParams = do
  let filter = (\(p, v) -> p =: v) <$> queryParams
  let action = delete $ select filter pkgCollection
  runMongoDBPoolDef action (context ^. ctxDbPool)

deletePackageById :: Context -> String -> IO ()
deletePackageById context pkgId = do
  let action = delete $ select ["id" =: pkgId] pkgCollection
  runMongoDBPoolDef action (context ^. ctxDbPool)
