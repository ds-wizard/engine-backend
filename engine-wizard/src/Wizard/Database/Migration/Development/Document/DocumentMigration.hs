module Wizard.Database.Migration.Development.Document.DocumentMigration where

import Control.Lens ((^.))
import qualified Data.UUID as U

import LensesConfig
import Shared.Constant.Component
import Wizard.Database.DAO.Document.DocumentDAO
import Wizard.Database.DAO.Document.DocumentQueueDAO
import Wizard.Database.Migration.Development.Document.Data.Documents
import Wizard.S3.Document.DocumentS3
import Wizard.Util.Logger

runMigration = do
  logInfo _CMP_MIGRATION "(Document/Document) started"
  deleteDocumentQueues
  deleteDocuments
  removeDocumentContents
  insertDocument doc1
  insertDocument doc2
  insertDocument doc3
  insertDocument differentDoc
  putDocumentContent (U.toString $ doc1 ^. uuid) doc1Content
  logInfo _CMP_MIGRATION "(Document/Document) ended"
