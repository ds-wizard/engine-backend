module Database.DAO.Package.PackageDAO where

import Control.Lens ((^.))
import Data.Bson
import Data.Bson.Generic
import Data.Maybe
import Database.MongoDB
       (find, findOne, select, insert, fetch, save, merge, delete,
        deleteOne, (=:), rest)
import Database.Persist.MongoDB (runMongoDBPoolDef)

import Common.Types
import Context
import Database.BSON.Package.Package
import Database.DAO.Common
import Model.Package.Package

pkgCollection = "packages"

findPackages :: Context -> IO [Package]
findPackages context = do
  let action = rest =<< find (select [] pkgCollection)
  packages <- runMongoDBPoolDef action (context ^. ctxDbPool)
  return $ fmap (fromJust . fromBSON) packages

findPackagesByName :: Context -> String -> IO [Package]
findPackagesByName context shortName = do
  let action = rest =<< find (select ["shortName" =: shortName] pkgCollection)
  packages <- runMongoDBPoolDef action (context ^. ctxDbPool)
  return $ fmap (fromJust . fromBSON) packages

findPackageByNameAndVersion :: Context -> String -> String -> IO (Maybe Package)
findPackageByNameAndVersion context shortName version = do
  let action =
        findOne $
        select ["shortName" =: shortName, "version" =: version] pkgCollection
  maybePackage <- runMongoDBPoolDef action (context ^. ctxDbPool)
  case maybePackage of
    Just package -> return . fromBSON $ package
    Nothing -> return Nothing

insertPackage :: Context -> Package -> IO Value
insertPackage context package = do
  let action = insert pkgCollection (toBSON package)
  runMongoDBPoolDef action (context ^. ctxDbPool)

--updatePackageById :: Context -> Package -> IO ()
--updatePackageById context package = do
--  let action =
--        fetch (select ["uuid" =: (package ^. pkgId)] pkgCollection) >>=
--        save pkgCollection . merge (toBSON package)
--  runMongoDBPoolDef action (context ^. ctxDbPool)
deletePackages :: Context -> IO ()
deletePackages context = do
  let action = delete $ select [] pkgCollection
  runMongoDBPoolDef action (context ^. ctxDbPool)

deletePackagesByName :: Context -> String -> IO ()
deletePackagesByName context shortName = do
  let action = delete $ select ["shortName" =: shortName] pkgCollection
  runMongoDBPoolDef action (context ^. ctxDbPool)

deletePackageByNameAndVersion :: Context -> String -> String -> IO ()
deletePackageByNameAndVersion context shortName version = do
  let action =
        deleteOne $
        select ["shortName" =: shortName, "version" =: version] pkgCollection
  runMongoDBPoolDef action (context ^. ctxDbPool)
