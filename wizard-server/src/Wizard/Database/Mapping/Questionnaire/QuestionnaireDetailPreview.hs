module Wizard.Database.Mapping.Questionnaire.QuestionnaireDetailPreview where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

import Shared.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateJM ()
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateFormatSimple
import Wizard.Api.Resource.Questionnaire.QuestionnairePermJM ()
import Wizard.Database.Mapping.Questionnaire.QuestionnaireAcl
import Wizard.Database.Mapping.Questionnaire.QuestionnaireSharing ()
import Wizard.Database.Mapping.Questionnaire.QuestionnaireVisibility ()
import Wizard.Model.Questionnaire.QuestionnaireDetailPreview

instance FromRow QuestionnaireDetailPreview where
  fromRow = do
    uuid <- field
    name <- field
    visibility <- field
    sharing <- field
    knowledgeModelPackageId <- field
    isTemplate <- field
    documentTemplateId <- field
    migrationUuid <- field
    permissions <- loadPermissions uuid
    mFormatUuid <- field
    mFormatName <- field
    mFormatIcon <- field
    let format =
          case (mFormatUuid, mFormatName, mFormatIcon) of
            (Just uuid, Just name, Just icon) -> Just $ DocumentTemplateFormatSimple {uuid = uuid, name = name, icon = icon}
            _ -> Nothing
    fileCount <- field
    return $ QuestionnaireDetailPreview {..}
