module Database.DAO.Package.PackageDAO where

import Control.Lens ((^.))
import Data.Bson
import Data.Bson.Generic
import Data.Maybe
import Data.Text (Text)
import Database.MongoDB
       (find, findOne, select, insert, fetch, save, merge, delete,
        deleteOne, (=:), rest)
import Database.Persist.MongoDB (runMongoDBPoolDef)

import Common.Types
import Context
import Database.BSON.Package.Package
import Database.BSON.Package.PackageWithEvents
import Database.DAO.Common
import Model.Package.Package

pkgCollection = "packages"

findPackages :: Context -> IO [Package]
findPackages context = do
  let action = rest =<< find (select [] pkgCollection)
  packages <- runMongoDBPoolDef action (context ^. ctxDbPool)
  return $ fmap (fromJust . fromBSON) packages

findPackagesFiltered :: Context -> [(Text, Text)] -> IO [Package]
findPackagesFiltered context queryParams = do
  let filter = (\(p, v) -> p =: v) <$> queryParams
  let action = rest =<< find (select filter pkgCollection)
  packages <- runMongoDBPoolDef action (context ^. ctxDbPool)
  return $ fmap (fromJust . fromBSON) packages

findPackagesWitParams :: Context -> IO [Package]
findPackagesWitParams context = do
  let action = rest =<< find (select [] pkgCollection)
  packages <- runMongoDBPoolDef action (context ^. ctxDbPool)
  return $ fmap (fromJust . fromBSON) packages

findPackageById :: Context -> String -> IO (Maybe Package)
findPackageById context pkgId = do
  let action = findOne $ select ["id" =: pkgId] pkgCollection
  maybePackage <- runMongoDBPoolDef action (context ^. ctxDbPool)
  case maybePackage of
    Just package -> return . fromBSON $ package
    Nothing -> return Nothing

findPackagesByArtifactId :: Context -> String -> IO [Package]
findPackagesByArtifactId context artifactId = do
  let action = rest =<< find (select ["artifactId" =: artifactId] pkgCollection)
  packages <- runMongoDBPoolDef action (context ^. ctxDbPool)
  return $ fmap (fromJust . fromBSON) packages

findPackageWithEventsById :: Context -> String -> IO (Maybe PackageWithEvents)
findPackageWithEventsById context pkgId = do
  let action = findOne $ select ["id" =: pkgId] pkgCollection
  maybePackage <- runMongoDBPoolDef action (context ^. ctxDbPool)
  case maybePackage of
    Just package -> return . fromBSON $ package
    Nothing -> return Nothing

insertPackage :: Context -> PackageWithEvents -> IO Value
insertPackage context package = do
  let action = insert pkgCollection (toBSON package)
  runMongoDBPoolDef action (context ^. ctxDbPool)

deletePackages :: Context -> IO ()
deletePackages context = do
  let action = delete $ select [] pkgCollection
  runMongoDBPoolDef action (context ^. ctxDbPool)

deletePackagesByArtifactId :: Context -> String -> IO ()
deletePackagesByArtifactId context artifactId = do
  let action = delete $ select ["artifactId" =: artifactId] pkgCollection
  runMongoDBPoolDef action (context ^. ctxDbPool)

deletePackageById :: Context -> String -> IO ()
deletePackageById context pkgId = do
  let action = deleteOne $ select ["id" =: pkgId] pkgCollection
  runMongoDBPoolDef action (context ^. ctxDbPool)
