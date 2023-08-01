module Wizard.Database.Migration.Development.Document.DocumentMigration where

import Shared.Common.Constant.Component
import Shared.Common.Util.Logger
import Wizard.Database.DAO.Document.DocumentDAO
import Wizard.Database.Migration.Development.Document.Data.Documents
import Wizard.Model.Document.Document
import Wizard.S3.Document.DocumentS3

runMigration = do
  logInfo _CMP_MIGRATION "(Document/Document) started"
  deleteDocuments
  removeDocumentContents
  insertDocument doc1
  insertDocument doc2
  insertDocument doc3
  insertDocument differentDoc
  putDocumentContent doc1.uuid doc1Content
  logInfo _CMP_MIGRATION "(Document/Document) ended"
