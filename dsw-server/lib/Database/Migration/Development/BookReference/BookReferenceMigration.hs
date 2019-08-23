module Database.Migration.Development.BookReference.BookReferenceMigration where

import Constant.Component
import Database.DAO.BookReference.BookReferenceDAO
import Database.Migration.Development.BookReference.Data.BookReferences
import Util.Logger

runMigration = do
  logInfo $ msg _CMP_MIGRATION "(BookReference/BookReference) started"
  deleteBookReferences
  insertBookReference bookReferenceBvq
  logInfo $ msg _CMP_MIGRATION "(BookReference/BookReference) ended"
