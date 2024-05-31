module Wizard.Database.Mapping.Document.DocumentList where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow

import Wizard.Database.Mapping.Document.Document ()
import Wizard.Model.Document.DocumentList
import WizardLib.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateJM ()

instance FromRow DocumentList where
  fromRow = do
    uuid <- field
    name <- field
    state <- field
    questionnaireUuid <- field
    questionnaireName <- field
    questionnaireEventUuid <- field
    questionnaireVersion <- field
    documentTemplateName <- field
    documentTemplateFormats <- fieldWith fromJSONField
    formatUuid <- field
    fileSize <- field
    workerLog <- field
    createdBy <- field
    createdAt <- field
    return $ DocumentList {..}
