module Wizard.Database.Mapping.Questionnaire.QuestionnaireDetailPreview where

import qualified Data.List as L
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow

import Shared.Common.Util.Maybe
import Wizard.Api.Resource.Questionnaire.QuestionnairePermJM ()
import Wizard.Database.Mapping.Questionnaire.QuestionnaireAcl
import Wizard.Database.Mapping.Questionnaire.QuestionnaireSharing ()
import Wizard.Database.Mapping.Questionnaire.QuestionnaireVisibility ()
import Wizard.Model.Questionnaire.QuestionnaireDetailPreview
import WizardLib.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateFormatDTO
import WizardLib.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateJM ()

instance FromRow QuestionnaireDetailPreview where
  fromRow = do
    uuid <- field
    name <- field
    visibility <- field
    sharing <- field
    packageId <- field
    isTemplate <- field
    documentTemplateId <- field
    mFormatUuid <- field
    migrationUuid <- field
    permissions <- loadPermissions uuid
    mDocumentTemplateFormats <- fieldWith (optionalField fromJSONField)
    let format = concatMaybe . fmap (L.find (\format -> Just format.uuid == mFormatUuid)) $ mDocumentTemplateFormats
    fileCount <- field
    return $ QuestionnaireDetailPreview {..}
