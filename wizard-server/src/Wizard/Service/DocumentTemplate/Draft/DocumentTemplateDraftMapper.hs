module Wizard.Service.DocumentTemplate.Draft.DocumentTemplateDraftMapper where

import Data.Time
import qualified Data.UUID as U

import Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftChangeDTO
import Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftCreateDTO
import Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftDataChangeDTO
import Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftDataDTO
import Wizard.Model.Branch.BranchSuggestion
import Wizard.Model.DocumentTemplate.DocumentTemplateDraftData
import Wizard.Model.DocumentTemplate.DocumentTemplateDraftDetail
import Wizard.Model.DocumentTemplate.DocumentTemplateDraftList
import Wizard.Model.Questionnaire.QuestionnaireSuggestion
import WizardLib.Common.Util.Coordinate
import WizardLib.DocumentTemplate.Constant.DocumentTemplate
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate

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

toDraftDetail :: DocumentTemplate -> DocumentTemplateDraftData -> Maybe QuestionnaireSuggestion -> Maybe BranchSuggestion -> DocumentTemplateDraftDetail
toDraftDetail draft draftData mQuestionnaire mBranch =
  DocumentTemplateDraftDetail
    { tId = draft.tId
    , name = draft.name
    , templateId = draft.templateId
    , version = draft.version
    , description = draft.description
    , readme = draft.readme
    , license = draft.license
    , allowedPackages = draft.allowedPackages
    , formats = draft.formats
    , questionnaireUuid = draftData.questionnaireUuid
    , questionnaire = mQuestionnaire
    , branchUuid = draftData.branchUuid
    , branch = mBranch
    , formatUuid = draftData.formatUuid
    , createdAt = draft.createdAt
    , updatedAt = draft.updatedAt
    }

toDraftDetail' :: DocumentTemplate -> DocumentTemplateDraftDetail
toDraftDetail' draft =
  DocumentTemplateDraftDetail
    { tId = draft.tId
    , name = draft.name
    , templateId = draft.templateId
    , version = draft.version
    , description = draft.description
    , readme = draft.readme
    , license = draft.license
    , allowedPackages = draft.allowedPackages
    , formats = draft.formats
    , questionnaireUuid = Nothing
    , questionnaire = Nothing
    , branchUuid = Nothing
    , branch = Nothing
    , formatUuid = Nothing
    , createdAt = draft.createdAt
    , updatedAt = draft.updatedAt
    }

toDraftDataDTO :: DocumentTemplateDraftData -> Maybe QuestionnaireSuggestion -> Maybe BranchSuggestion -> DocumentTemplateDraftDataDTO
toDraftDataDTO draftData mQuestionnaire mBranch =
  DocumentTemplateDraftDataDTO
    { questionnaireUuid = draftData.questionnaireUuid
    , questionnaire = mQuestionnaire
    , branchUuid = draftData.branchUuid
    , branch = mBranch
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
    , formats = tml.formats
    }

fromCreateDTO :: DocumentTemplateDraftCreateDTO -> DocumentTemplate -> String -> UTCTime -> DocumentTemplate
fromCreateDTO dto tml organizationId now =
  DocumentTemplate
    { tId = buildCoordinate organizationId dto.templateId dto.version
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
    , formats = tml.formats
    , nonEditable = False
    , tenantUuid = tml.tenantUuid
    , createdAt = now
    , updatedAt = now
    }

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
    , formats = []
    , nonEditable = False
    , tenantUuid = tenantUuid
    , createdAt = now
    , updatedAt = now
    }

fromChangeDTO :: DocumentTemplateDraftChangeDTO -> DocumentTemplate -> DocumentTemplate
fromChangeDTO dto tml =
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
    , formats = dto.formats
    , nonEditable = tml.nonEditable
    , tenantUuid = tml.tenantUuid
    , createdAt = tml.createdAt
    , updatedAt = tml.updatedAt
    }

fromCreateDraftData :: DocumentTemplate -> DocumentTemplateDraftData
fromCreateDraftData draft =
  DocumentTemplateDraftData
    { documentTemplateId = draft.tId
    , questionnaireUuid = Nothing
    , branchUuid = Nothing
    , formatUuid = Nothing
    , tenantUuid = draft.tenantUuid
    , createdAt = draft.createdAt
    , updatedAt = draft.updatedAt
    }

fromDraftDataChangeDTO :: DocumentTemplateDraftData -> DocumentTemplateDraftDataChangeDTO -> DocumentTemplateDraftData
fromDraftDataChangeDTO draftData reqDto =
  DocumentTemplateDraftData
    { documentTemplateId = draftData.documentTemplateId
    , questionnaireUuid = reqDto.questionnaireUuid
    , branchUuid = reqDto.branchUuid
    , formatUuid = reqDto.formatUuid
    , tenantUuid = draftData.tenantUuid
    , createdAt = draftData.createdAt
    , updatedAt = draftData.updatedAt
    }
