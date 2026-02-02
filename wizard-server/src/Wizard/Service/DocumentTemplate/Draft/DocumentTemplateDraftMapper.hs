module Wizard.Service.DocumentTemplate.Draft.DocumentTemplateDraftMapper where

import Data.Time
import qualified Data.UUID as U

import Shared.Coordinate.Util.Coordinate
import Shared.DocumentTemplate.Constant.DocumentTemplate
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftChangeDTO
import Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftCreateDTO
import Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftDataChangeDTO
import Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftDataDTO
import Wizard.Model.DocumentTemplate.DocumentTemplateDraftData
import Wizard.Model.DocumentTemplate.DocumentTemplateDraftDetail
import Wizard.Model.DocumentTemplate.DocumentTemplateDraftList
import Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditorSuggestion
import Wizard.Model.Project.ProjectSuggestion

toDraftList :: DocumentTemplate -> DocumentTemplateDraftList
toDraftList dt =
  DocumentTemplateDraftList
    { uuid = dt.uuid
    , name = dt.name
    , organizationId = dt.organizationId
    , templateId = dt.templateId
    , version = dt.version
    , description = dt.description
    , createdAt = dt.createdAt
    , updatedAt = dt.updatedAt
    }

toDraftDetail :: DocumentTemplate -> [DocumentTemplateFormat] -> DocumentTemplateDraftData -> Maybe ProjectSuggestion -> Maybe KnowledgeModelEditorSuggestion -> DocumentTemplateDraftDetail
toDraftDetail draft formats draftData mProject mKmEditor =
  DocumentTemplateDraftDetail
    { uuid = draft.uuid
    , name = draft.name
    , templateId = draft.templateId
    , version = draft.version
    , description = draft.description
    , readme = draft.readme
    , license = draft.license
    , allowedPackages = draft.allowedPackages
    , formats = formats
    , projectUuid = draftData.projectUuid
    , project = mProject
    , knowledgeModelEditorUuid = draftData.knowledgeModelEditorUuid
    , knowledgeModelEditor = mKmEditor
    , formatUuid = draftData.formatUuid
    , createdAt = draft.createdAt
    , updatedAt = draft.updatedAt
    }

toDraftDetail' :: DocumentTemplate -> [DocumentTemplateFormat] -> DocumentTemplateDraftDetail
toDraftDetail' draft formats =
  DocumentTemplateDraftDetail
    { uuid = draft.uuid
    , name = draft.name
    , templateId = draft.templateId
    , version = draft.version
    , description = draft.description
    , readme = draft.readme
    , license = draft.license
    , allowedPackages = draft.allowedPackages
    , formats = formats
    , projectUuid = Nothing
    , project = Nothing
    , knowledgeModelEditorUuid = Nothing
    , knowledgeModelEditor = Nothing
    , formatUuid = Nothing
    , createdAt = draft.createdAt
    , updatedAt = draft.updatedAt
    }

toDraftDataDTO :: DocumentTemplateDraftData -> Maybe ProjectSuggestion -> Maybe KnowledgeModelEditorSuggestion -> DocumentTemplateDraftDataDTO
toDraftDataDTO draftData mProject mKmEditor =
  DocumentTemplateDraftDataDTO
    { projectUuid = draftData.projectUuid
    , project = mProject
    , knowledgeModelEditorUuid = draftData.knowledgeModelEditorUuid
    , knowledgeModelEditor = mKmEditor
    , formatUuid = draftData.formatUuid
    }

toChangeDTO :: DocumentTemplate -> DocumentTemplateDraftChangeDTO
toChangeDTO dt =
  DocumentTemplateDraftChangeDTO
    { name = dt.name
    , templateId = dt.templateId
    , version = dt.version
    , phase = dt.phase
    , description = dt.description
    , readme = dt.readme
    , license = dt.license
    , allowedPackages = dt.allowedPackages
    , formats = []
    }

fromCreateDTO :: DocumentTemplateDraftCreateDTO -> U.UUID -> DocumentTemplate -> [DocumentTemplateFormat] -> String -> UTCTime -> (DocumentTemplate, [DocumentTemplateFormat])
fromCreateDTO dto uuid dt formats organizationId now =
  let documentTemplateUuid = buildCoordinate organizationId dto.templateId dto.version
   in ( DocumentTemplate
          { uuid = uuid
          , name = dto.name
          , organizationId = organizationId
          , templateId = dto.templateId
          , version = dto.version
          , phase = DraftDocumentTemplatePhase
          , metamodelVersion = documentTemplateMetamodelVersion
          , description = dt.description
          , readme = dt.readme
          , license = dt.license
          , allowedPackages = dt.allowedPackages
          , nonEditable = False
          , tenantUuid = dt.tenantUuid
          , createdAt = now
          , updatedAt = now
          }
      , fmap
          ( \f ->
              f
                { documentTemplateUuid = uuid
                , steps =
                    fmap
                      ( \s ->
                          s
                            { documentTemplateUuid = uuid
                            , createdAt = now
                            , updatedAt = now
                            }
                          :: DocumentTemplateFormatStep
                      )
                      (steps f)
                , createdAt = now
                , updatedAt = now
                }
              :: DocumentTemplateFormat
          )
          formats
      )

fromCreateDTO' :: DocumentTemplateDraftCreateDTO -> U.UUID -> String -> U.UUID -> UTCTime -> DocumentTemplate
fromCreateDTO' dto uuid organizationId tenantUuid now =
  DocumentTemplate
    { uuid = uuid
    , name = dto.name
    , organizationId = organizationId
    , templateId = dto.templateId
    , version = dto.version
    , phase = DraftDocumentTemplatePhase
    , metamodelVersion = documentTemplateMetamodelVersion
    , description = ""
    , readme = ""
    , license = ""
    , allowedPackages = []
    , nonEditable = False
    , tenantUuid = tenantUuid
    , createdAt = now
    , updatedAt = now
    }

fromChangeDTO :: DocumentTemplateDraftChangeDTO -> DocumentTemplate -> UTCTime -> DocumentTemplate
fromChangeDTO dto dt now =
  DocumentTemplate
    { uuid = dt.uuid
    , name = dto.name
    , organizationId = dt.organizationId
    , templateId = dto.templateId
    , version = dto.version
    , phase = dto.phase
    , metamodelVersion = dt.metamodelVersion
    , description = dto.description
    , readme = dto.readme
    , license = dto.license
    , allowedPackages = dto.allowedPackages
    , nonEditable = dt.nonEditable
    , tenantUuid = dt.tenantUuid
    , createdAt = now
    , updatedAt = now
    }

fromCreateDraftData :: DocumentTemplate -> DocumentTemplateDraftData
fromCreateDraftData draft =
  DocumentTemplateDraftData
    { documentTemplateUuid = draft.uuid
    , projectUuid = Nothing
    , knowledgeModelEditorUuid = Nothing
    , formatUuid = Nothing
    , tenantUuid = draft.tenantUuid
    , createdAt = draft.createdAt
    , updatedAt = draft.updatedAt
    }

fromDraftDataChangeDTO :: DocumentTemplateDraftData -> DocumentTemplateDraftDataChangeDTO -> DocumentTemplateDraftData
fromDraftDataChangeDTO draftData reqDto =
  DocumentTemplateDraftData
    { documentTemplateUuid = draftData.documentTemplateUuid
    , projectUuid = reqDto.projectUuid
    , knowledgeModelEditorUuid = reqDto.knowledgeModelEditorUuid
    , formatUuid = reqDto.formatUuid
    , tenantUuid = draftData.tenantUuid
    , createdAt = draftData.createdAt
    , updatedAt = draftData.updatedAt
    }
