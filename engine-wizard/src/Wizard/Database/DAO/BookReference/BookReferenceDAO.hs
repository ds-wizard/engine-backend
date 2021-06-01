module Wizard.Database.DAO.BookReference.BookReferenceDAO where

import GHC.Int

import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.BookReference.BookReference ()
import Wizard.Model.BookReference.BookReference
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

entityName = "book_reference"

findBookReferences :: AppContextM [BookReference]
findBookReferences = createFindEntitiesFn entityName

findBookReferenceByShortUuid :: String -> AppContextM BookReference
findBookReferenceByShortUuid = createFindEntityByFn entityName "short_uuid"

insertBookReference :: BookReference -> AppContextM Int64
insertBookReference = createInsertFn entityName

deleteBookReferences :: AppContextM Int64
deleteBookReferences = createDeleteEntitiesFn entityName

deleteBookReferenceByShortUuid :: String -> AppContextM Int64
deleteBookReferenceByShortUuid = createDeleteEntityByFn entityName "short_uuid"
