module Wizard.Database.Mapping.Questionnaire.QuestionnaireDetailSettings where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.Types

import Shared.Common.Database.Mapping.Common.SemVer2Tuple ()
import Shared.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateDTO
import Shared.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateJM ()
import Shared.DocumentTemplate.Constant.DocumentTemplate
import Shared.DocumentTemplate.Database.Mapping.DocumentTemplate.DocumentTemplatePhase ()
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateJM ()
import Shared.KnowledgeModel.Database.Mapping.KnowledgeModel.Package.KnowledgeModelPackagePhase ()
import Wizard.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleDTO
import Wizard.Api.Resource.Questionnaire.QuestionnairePermJM ()
import Wizard.Database.Mapping.Questionnaire.QuestionnaireAcl
import Wizard.Database.Mapping.Questionnaire.QuestionnaireSharing ()
import Wizard.Database.Mapping.Questionnaire.QuestionnaireState ()
import Wizard.Database.Mapping.Questionnaire.QuestionnaireVisibility ()
import Wizard.Model.DocumentTemplate.DocumentTemplateState
import Wizard.Model.Questionnaire.QuestionnaireDetailSettings

instance FromRow QuestionnaireDetailSettings where
  fromRow = do
    uuid <- field
    name <- field
    description <- field
    visibility <- field
    sharing <- field
    isTemplate <- field
    projectTags <- fromPGArray <$> field
    selectedQuestionTagUuids <- fromPGArray <$> field
    formatUuid <- field
    migrationUuid <- field
    permissions <- loadPermissions uuid
    knowledgeModelPackageId <- field
    knowledgeModelPackageName <- field
    knowledgeModelPackageOrganizationId <- field
    knowledgeModelPackageKmId <- field
    knowledgeModelPackageVersion <- field
    knowledgeModelPackagePhase <- field
    knowledgeModelPackageDescription <- field
    knowledgeModelPackageNonEditable <- field
    knowledgeModelPackageCreatedAt <- field
    let knowledgeModelPackage =
          KnowledgeModelPackageSimpleDTO
            { pId = knowledgeModelPackageId
            , name = knowledgeModelPackageName
            , organizationId = knowledgeModelPackageOrganizationId
            , kmId = knowledgeModelPackageKmId
            , version = knowledgeModelPackageVersion
            , phase = knowledgeModelPackagePhase
            , remoteLatestVersion = Nothing
            , description = knowledgeModelPackageDescription
            , organization = Nothing
            , nonEditable = knowledgeModelPackageNonEditable
            , createdAt = knowledgeModelPackageCreatedAt
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
    fileCount <- field
    return $ QuestionnaireDetailSettings {..}
