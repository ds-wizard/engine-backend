module Wizard.Database.DAO.PublicPackage.PublicPackageDAO where

import Data.Bson

import Shared.Model.Package.PackageWithEvents
import Wizard.Database.BSON.Package.PackageWithEvents ()
import Wizard.Database.DAO.Common
import Wizard.Model.Context.AppContext

entityName = "publicPackage"

collection = "publicPackages"

findPublicPackage' :: AppContextM (Maybe PackageWithEvents)
findPublicPackage' = createFindEntityFn' collection entityName

insertPublicPackage :: PackageWithEvents -> AppContextM Value
insertPublicPackage = createInsertFn collection

updatePublicPackage :: PackageWithEvents -> AppContextM ()
updatePublicPackage = createUpdateFn collection

deletePublicPackages :: AppContextM ()
deletePublicPackages = createDeleteEntitiesFn collection
