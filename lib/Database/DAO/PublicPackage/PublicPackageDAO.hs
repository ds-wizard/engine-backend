module Database.DAO.PublicPackage.PublicPackageDAO where

import Data.Bson
import Data.Bson.Generic
import Database.MongoDB
       (delete, fetch, findOne, insert, merge, save, select)

import Database.BSON.Package.PackageWithEvents ()
import Database.DAO.Common
import Model.Context.AppContext
import Model.Error.Error
import Model.Package.PackageWithEvents

entityName = "publicPackage"

pubPkgCollection = "publicPackages"

findPublicPackage :: AppContextM (Either AppError PackageWithEvents)
findPublicPackage = do
  let action = findOne $ select [] pubPkgCollection
  maybePublicPackage <- runDB action
  return . deserializeMaybeEntity entityName "nothing" $ maybePublicPackage

insertPublicPackage :: PackageWithEvents -> AppContextM Value
insertPublicPackage pubPkg = do
  let action = insert pubPkgCollection (toBSON pubPkg)
  runDB action

updatePublicPackage :: PackageWithEvents -> AppContextM ()
updatePublicPackage pubPkg = do
  let action = fetch (select [] pubPkgCollection) >>= save pubPkgCollection . merge (toBSON pubPkg)
  runDB action

deletePublicPackages :: AppContextM ()
deletePublicPackages = do
  let action = delete $ select [] pubPkgCollection
  runDB action
