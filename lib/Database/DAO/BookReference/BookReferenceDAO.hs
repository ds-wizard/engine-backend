module Database.DAO.BookReference.BookReferenceDAO where

import Data.Bson
import Data.Bson.Generic
import Database.MongoDB
       ((=:), delete, deleteOne, find, findOne, insert, rest, select)

import Common.Error
import Database.BSON.BookReference.BookReference ()
import Database.DAO.Common
import Model.BookReference.BookReference
import Model.Context.AppContext

bookReferenceCollection = "bookReferences"

findBookReferences :: AppContextM (Either AppError [BookReference])
findBookReferences = do
  let action = rest =<< find (select [] bookReferenceCollection)
  bookReferencesS <- runDB action
  return . deserializeEntities $ bookReferencesS

findBookReferenceByShortUuid :: String -> AppContextM (Either AppError BookReference)
findBookReferenceByShortUuid shortUuid = do
  let action = findOne $ select ["shortUuid" =: shortUuid] bookReferenceCollection
  maybeBookReferenceS <- runDB action
  return . deserializeMaybeEntity $ maybeBookReferenceS

insertBookReference :: BookReference -> AppContextM Value
insertBookReference bookReference = do
  let action = insert bookReferenceCollection (toBSON bookReference)
  runDB action

deleteBookReferences :: AppContextM ()
deleteBookReferences = do
  let action = delete $ select [] bookReferenceCollection
  runDB action

deleteBookReferenceByShortUuid :: String -> AppContextM ()
deleteBookReferenceByShortUuid shortUuid = do
  let action = deleteOne $ select ["shortUuid" =: shortUuid] bookReferenceCollection
  runDB action

-- --------------------------------
-- HELPERS
-- --------------------------------
heFindBookReferenceByShortUuid shortUuid callback = do
  eitherBookReference <- findBookReferenceByShortUuid shortUuid
  case eitherBookReference of
    Right actionKey -> callback actionKey
    Left error -> return . Left $ error
