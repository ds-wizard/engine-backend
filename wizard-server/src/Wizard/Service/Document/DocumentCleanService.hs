module Wizard.Service.Document.DocumentCleanService where

import Control.Monad (void)
import Data.Foldable (traverse_)

import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Document.DocumentDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.Document.Document
import Wizard.S3.Document.DocumentS3

cleanDocuments :: AppContextM ()
cleanDocuments =
  runInTransaction $ do
    docs <- findDocumentsFiltered [("durability", "TemporallyDocumentDurability")]
    let docsFiltered = filter (\d -> d.state == DoneDocumentState || d.state == ErrorDocumentState) docs
    traverse_
      ( \d -> do
          deleteDocumentByUuidAndTenantUuid d.uuid d.tenantUuid
          removeDocumentContentWithTenant d.tenantUuid d.uuid
      )
      docsFiltered

cleanTemporallyDocumentsForTemplate :: String -> AppContextM ()
cleanTemporallyDocumentsForTemplate documentTemplateId =
  void $ deleteDocumentsFiltered [("document_template_id", documentTemplateId), ("durability", "TemporallyDocumentDurability")]
