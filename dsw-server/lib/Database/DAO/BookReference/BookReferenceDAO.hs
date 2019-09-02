module Database.DAO.BookReference.BookReferenceDAO where

import Data.Bson

import Database.BSON.BookReference.BookReference ()
import Database.DAO.Common
import Model.BookReference.BookReference
import Model.Context.AppContext
import Model.Error.Error
import Util.Helper (createHeeHelper)

entityName = "bookReference"

collection = "bookReferences"

findBookReferences :: AppContextM (Either AppError [BookReference])
findBookReferences = createFindEntitiesFn collection

findBookReferenceByShortUuid :: String -> AppContextM (Either AppError BookReference)
findBookReferenceByShortUuid = createFindEntityByFn collection entityName "shortUuid"

insertBookReference :: BookReference -> AppContextM Value
insertBookReference = createInsertFn collection

deleteBookReferences :: AppContextM ()
deleteBookReferences = createDeleteEntitiesFn collection

deleteBookReferenceByShortUuid :: String -> AppContextM ()
deleteBookReferenceByShortUuid = createDeleteEntityByFn collection "shortUuid"

-- --------------------------------
-- HELPERS
-- --------------------------------
heFindBookReferenceByShortUuid shortUuid callback = createHeeHelper (findBookReferenceByShortUuid shortUuid) callback
