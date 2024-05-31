module Wizard.Database.Mapping.Questionnaire.QuestionnaireDetailSettings where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.Types

import Wizard.Api.Resource.Package.PackageSimpleDTO
import Wizard.Api.Resource.Questionnaire.QuestionnairePermJM ()
import Wizard.Database.Mapping.Questionnaire.QuestionnaireAcl
import Wizard.Database.Mapping.Questionnaire.QuestionnaireSharing ()
import Wizard.Database.Mapping.Questionnaire.QuestionnaireState ()
import Wizard.Database.Mapping.Questionnaire.QuestionnaireVisibility ()
import Wizard.Model.DocumentTemplate.DocumentTemplateState
import Wizard.Model.Questionnaire.QuestionnaireDetailSettings
import WizardLib.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateDTO
import WizardLib.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateJM ()
import WizardLib.DocumentTemplate.Constant.DocumentTemplate
import WizardLib.DocumentTemplate.Database.Mapping.DocumentTemplate.DocumentTemplatePhase ()
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateJM ()
import WizardLib.KnowledgeModel.Database.Mapping.Package.PackagePhase ()

instance FromRow QuestionnaireDetailSettings where
  fromRow = do
    uuid <- field
    name <- field
    description <- field
    visibility <- field
    sharing <- field
    isTemplate <- field
    projectTags <- fromPGArray <$> field
    selectedQuestionTagUuids <- fieldWith fromJSONField
    formatUuid <- field
    migrationUuid <- field
    permissions <- loadPermissions uuid
    packageId <- field
    packageName <- field
    packageOrganizationId <- field
    packageKmId <- field
    packageVersion <- field
    packagePhase <- field
    packageDescription <- field
    packageNonEditable <- field
    packageCreatedAt <- field
    let package =
          PackageSimpleDTO
            { pId = packageId
            , name = packageName
            , organizationId = packageOrganizationId
            , kmId = packageKmId
            , version = packageVersion
            , phase = packagePhase
            , remoteLatestVersion = Nothing
            , description = packageDescription
            , organization = Nothing
            , nonEditable = packageNonEditable
            , createdAt = packageCreatedAt
            }
    let knowledgeModelTags = []
    mDocumentTemplateId <- field
    mDocumentTemplateName <- field
    mDocumentTemplateVersion <- field
    mDocumentTemplatePhase <- field
    mDocumentTemplateDescription <- field
    mDocumentTemplateFormats <- fieldWith (optionalField fromJSONField)
    let documentTemplate =
          case (mDocumentTemplateId, mDocumentTemplateName, mDocumentTemplateVersion, mDocumentTemplatePhase, mDocumentTemplateDescription, mDocumentTemplateFormats) of
            (Just documentTemplateId, Just documentTemplateName, Just documentTemplateVersion, Just documentTemplatePhase, Just documentTemplateDescription, Just documentTemplateFormats) ->
              Just $
                DocumentTemplateDTO
                  { tId = documentTemplateId
                  , name = documentTemplateName
                  , version = documentTemplateVersion
                  , phase = documentTemplatePhase
                  , description = documentTemplateDescription
                  , formats = documentTemplateFormats
                  }
            _ -> Nothing
    let documentTemplatePhase = mDocumentTemplatePhase
    mDocumentTemplateMetamodelVersion <- field
    let documentTemplateState =
          case mDocumentTemplateMetamodelVersion of
            Just metamodelVersion ->
              if metamodelVersion /= documentTemplateMetamodelVersion
                then Just UnsupportedMetamodelVersionDocumentTemplateState
                else Just DefaultDocumentTemplateState
            _ -> Nothing

    return $ QuestionnaireDetailSettings {..}
