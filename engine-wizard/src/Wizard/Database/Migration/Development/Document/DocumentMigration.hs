module Wizard.Database.Migration.Development.Document.DocumentMigration where

import Control.Lens ((^.))
import qualified Data.UUID as U

import LensesConfig
import Shared.Constant.Component
import Wizard.Database.DAO.Document.DocumentDAO
import Wizard.Database.Migration.Development.Document.Data.Documents
import Wizard.Util.Logger

runMigration = do
  logInfo _CMP_MIGRATION "(Document/Document) started"
  deleteDocuments
  deleteDocumentContents
  insertDocument doc1
  insertDocument doc2
  insertDocument doc3
  insertDocumentContent (U.toString $ doc1 ^. uuid) doc1Content
  logInfo _CMP_MIGRATION "(Document/Document) ended"
