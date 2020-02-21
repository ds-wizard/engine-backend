module Wizard.Database.DAO.BookReference.BookReferenceDAO where

import Data.Bson

import Wizard.Database.BSON.BookReference.BookReference ()
import Wizard.Database.DAO.Common
import Wizard.Model.BookReference.BookReference
import Wizard.Model.Context.AppContext

entityName = "bookReference"

collection = "bookReferences"

findBookReferences :: AppContextM [BookReference]
findBookReferences = createFindEntitiesFn collection

findBookReferenceByShortUuid :: String -> AppContextM BookReference
findBookReferenceByShortUuid = createFindEntityByFn collection entityName "shortUuid"

insertBookReference :: BookReference -> AppContextM Value
insertBookReference = createInsertFn collection

deleteBookReferences :: AppContextM ()
deleteBookReferences = createDeleteEntitiesFn collection

deleteBookReferenceByShortUuid :: String -> AppContextM ()
deleteBookReferenceByShortUuid = createDeleteEntityByFn collection "shortUuid"
