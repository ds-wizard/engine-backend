module Wizard.Database.Migration.Development.BookReference.BookReferenceMigration where

import Shared.Common.Constant.Component
import Shared.Common.Util.Logger
import Wizard.Database.DAO.BookReference.BookReferenceDAO
import Wizard.Database.Migration.Development.BookReference.Data.BookReferences

runMigration = do
  logInfo _CMP_MIGRATION "(BookReference/BookReference) started"
  deleteBookReferences
  insertBookReference bookReferenceBvq
  logInfo _CMP_MIGRATION "(BookReference/BookReference) ended"
