module Wizard.Database.Migration.Development.BookReference.BookReferenceMigration where

import Wizard.Constant.Component
import Wizard.Database.DAO.BookReference.BookReferenceDAO
import Wizard.Database.Migration.Development.BookReference.Data.BookReferences
import Wizard.Util.Logger

runMigration = do
  logInfo $ msg _CMP_MIGRATION "(BookReference/BookReference) started"
  deleteBookReferences
  insertBookReference bookReferenceBvq
  logInfo $ msg _CMP_MIGRATION "(BookReference/BookReference) ended"
