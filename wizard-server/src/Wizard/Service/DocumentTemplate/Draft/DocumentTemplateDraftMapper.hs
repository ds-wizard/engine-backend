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
toDraftList tml =
  DocumentTemplateDraftList
    { tId = tml.tId
    , name = tml.name
    , organizationId = tml.organizationId
    , templateId = tml.templateId
    , version = tml.version
    , description = tml.description
    , createdAt = tml.createdAt
    , updatedAt = tml.updatedAt
    }

toDraftDetail :: DocumentTemplate -> [DocumentTemplateFormat] -> DocumentTemplateDraftData -> Maybe ProjectSuggestion -> Maybe KnowledgeModelEditorSuggestion -> DocumentTemplateDraftDetail
toDraftDetail draft formats draftData mProject mKmEditor =
  DocumentTemplateDraftDetail
    { tId = draft.tId
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
    { tId = draft.tId
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
toChangeDTO tml =
  DocumentTemplateDraftChangeDTO
    { name = tml.name
    , templateId = tml.templateId
    , version = tml.version
    , phase = tml.phase
    , description = tml.description
    , readme = tml.readme
    , license = tml.license
    , allowedPackages = tml.allowedPackages
    , formats = []
    }

fromCreateDTO :: DocumentTemplateDraftCreateDTO -> DocumentTemplate -> [DocumentTemplateFormat] -> String -> UTCTime -> (DocumentTemplate, [DocumentTemplateFormat])
fromCreateDTO dto tml formats organizationId now =
  let documentTemplateId = buildCoordinate organizationId dto.templateId dto.version
   in ( DocumentTemplate
          { tId = documentTemplateId
          , name = dto.name
          , organizationId = organizationId
          , templateId = dto.templateId
          , version = dto.version
          , phase = DraftDocumentTemplatePhase
          , metamodelVersion = documentTemplateMetamodelVersion
          , description = tml.description
          , readme = tml.readme
          , license = tml.license
          , allowedPackages = tml.allowedPackages
          , nonEditable = False
          , tenantUuid = tml.tenantUuid
          , createdAt = now
          , updatedAt = now
          }
      , fmap
          ( \f ->
              f
                { documentTemplateId = documentTemplateId
                , steps =
                    fmap
                      ( \s ->
                          s
                            { documentTemplateId = documentTemplateId
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

fromCreateDTO' :: DocumentTemplateDraftCreateDTO -> String -> U.UUID -> UTCTime -> DocumentTemplate
fromCreateDTO' dto organizationId tenantUuid now =
  DocumentTemplate
    { tId = buildCoordinate organizationId dto.templateId dto.version
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
fromChangeDTO dto tml now =
  DocumentTemplate
    { tId = tml.tId
    , name = dto.name
    , organizationId = tml.organizationId
    , templateId = tml.templateId
    , version = tml.version
    , phase = dto.phase
    , metamodelVersion = tml.metamodelVersion
    , description = dto.description
    , readme = dto.readme
    , license = dto.license
    , allowedPackages = dto.allowedPackages
    , nonEditable = tml.nonEditable
    , tenantUuid = tml.tenantUuid
    , createdAt = now
    , updatedAt = now
    }

fromCreateDraftData :: DocumentTemplate -> DocumentTemplateDraftData
fromCreateDraftData draft =
  DocumentTemplateDraftData
    { documentTemplateId = draft.tId
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
    { documentTemplateId = draftData.documentTemplateId
    , projectUuid = reqDto.projectUuid
    , knowledgeModelEditorUuid = reqDto.knowledgeModelEditorUuid
    , formatUuid = reqDto.formatUuid
    , tenantUuid = draftData.tenantUuid
    , createdAt = draftData.createdAt
    , updatedAt = draftData.updatedAt
    }
