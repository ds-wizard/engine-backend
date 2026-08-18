module Wizard.Database.Mapping.Project.ProjectDetailSettings where

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
import Wizard.Api.Resource.Project.Acl.ProjectPermJM ()
import Wizard.Database.Mapping.Project.ProjectAcl
import Wizard.Database.Mapping.Project.ProjectSharing ()
import Wizard.Database.Mapping.Project.ProjectState ()
import Wizard.Database.Mapping.Project.ProjectVisibility ()
import Wizard.Model.DocumentTemplate.DocumentTemplateState
import Wizard.Model.Project.Detail.ProjectDetailSettings

instance FromRow ProjectDetailSettings where
  fromRow = do
    uuid <- field
    name <- field
    description <- field
    visibility <- field
    sharing <- field
    isTemplate <- field
    projectTags <- fromPGArray <$> field
    selectedQuestionTagUuids <- fromPGArray <$> field
    language <- field
    formatUuid <- field
    permissions <- loadPermissions uuid
    knowledgeModelPackageUuid <- field
    knowledgeModelPackageName <- field
    knowledgeModelPackageOrganizationId <- field
    knowledgeModelPackageKmId <- field
    knowledgeModelPackageVersion <- field
    knowledgeModelPackagePhase <- field
    knowledgeModelPackageDescription <- field
    knowledgeModelPackageNonEditable <- field
    knowledgeModelPackagePublic <- field
    knowledgeModelPackageLanguage <- field
    knowledgeModelPackageCreatedAt <- field
    let knowledgeModelPackage =
          KnowledgeModelPackageSimpleDTO
            { uuid = knowledgeModelPackageUuid
            , name = knowledgeModelPackageName
            , organizationId = knowledgeModelPackageOrganizationId
            , kmId = knowledgeModelPackageKmId
            , version = knowledgeModelPackageVersion
            , phase = knowledgeModelPackagePhase
            , remoteLatestVersion = Nothing
            , description = knowledgeModelPackageDescription
            , organization = Nothing
            , nonEditable = knowledgeModelPackageNonEditable
            , public = knowledgeModelPackagePublic
            , language = knowledgeModelPackageLanguage
            , createdAt = knowledgeModelPackageCreatedAt
            }
    let knowledgeModelTags = []
    let availableLocales = []
    mDocumentTemplateId <- field
    mDocumentTemplateName <- field
    mDocumentTemplateOrganizationId <- field
    mDocumentTemplateTemplateId <- field
    mDocumentTemplateVersion <- field
    mDocumentTemplatePhase <- field
    mDocumentTemplateDescription <- field
    mDocumentTemplateFormats <- fieldWith (optionalField fromJSONField)
    let documentTemplate =
          case (mDocumentTemplateId, mDocumentTemplateName, mDocumentTemplateOrganizationId, mDocumentTemplateTemplateId, mDocumentTemplateVersion, mDocumentTemplatePhase, mDocumentTemplateDescription, mDocumentTemplateFormats) of
            (Just documentTemplateUuid, Just documentTemplateName, Just documentTemplateOrganizationId, Just documentTemplateTemplateId, Just documentTemplateVersion, Just documentTemplatePhase, Just documentTemplateDescription, Just documentTemplateFormats) ->
              Just $
                DocumentTemplateDTO
                  { uuid = documentTemplateUuid
                  , name = documentTemplateName
                  , organizationId = documentTemplateOrganizationId
                  , templateId = documentTemplateTemplateId
                  , version = documentTemplateVersion
                  , phase = documentTemplatePhase
                  , description = documentTemplateDescription
                  , formats = documentTemplateFormats
                  }
            _ -> Nothing
    let documentTemplatePhase = mDocumentTemplatePhase
    mDocumentTemplateMetamodelVersion <- field
    let documentTemplateSupportState =
          case mDocumentTemplateMetamodelVersion of
            Just metamodelVersion ->
              if isDocumentTemplateUnsupported metamodelVersion
                then Just UnsupportedMetamodelVersionDocumentTemplateState
                else Just DefaultDocumentTemplateState
            _ -> Nothing
    knowledgeModelState <- field
    documentTemplateState <- field
    fileCount <- field
    return $ ProjectDetailSettings {..}
