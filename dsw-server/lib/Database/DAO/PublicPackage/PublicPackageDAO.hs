module Database.DAO.PublicPackage.PublicPackageDAO where

import Data.Bson

import Database.BSON.Package.PackageWithEvents ()
import Database.DAO.Common
import Model.Context.AppContext
import Model.Error.Error
import Model.Package.PackageWithEvents

entityName = "publicPackage"

collection = "publicPackages"

findPublicPackage :: AppContextM (Either AppError PackageWithEvents)
findPublicPackage = createFindEntityFn collection entityName

insertPublicPackage :: PackageWithEvents -> AppContextM Value
insertPublicPackage = createInsertFn collection

updatePublicPackage :: PackageWithEvents -> AppContextM ()
updatePublicPackage = createUpdateFn collection

deletePublicPackages :: AppContextM ()
deletePublicPackages = createDeleteEntitiesFn collection
