module Database.Migration.Development.BookReference.BookReferenceMigration where

import Control.Monad.Logger (logInfo)

import Database.DAO.BookReference.BookReferenceDAO
import Database.Migration.Development.BookReference.Data.BookReferences

runMigration = do
  $(logInfo) "MIGRATION (BookReference/BookReference): started"
  deleteBookReferences
  insertBookReference bookReferenceBvq
  $(logInfo) "MIGRATION (BookReference/BookReference): ended"
