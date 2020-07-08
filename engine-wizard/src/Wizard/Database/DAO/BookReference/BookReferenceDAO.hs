module Wizard.Database.DAO.BookReference.BookReferenceDAO where

import Data.Bson

import Shared.Database.DAO.Common
import Wizard.Database.BSON.BookReference.BookReference ()
import Wizard.Model.BookReference.BookReference
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

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
