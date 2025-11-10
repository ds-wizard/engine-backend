module Wizard.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateDrafts where

import Shared.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFormats
import Shared.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftChangeDTO
import Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftCreateDTO
import Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftDataChangeDTO
import Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftDataDTO
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import Wizard.Model.DocumentTemplate.DocumentTemplateDraftData
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Service.DocumentTemplate.Draft.DocumentTemplateDraftMapper
import qualified Wizard.Service.Questionnaire.QuestionnaireMapper as QuestionnaireMapper

wizardDocumentTemplateDraftData :: DocumentTemplateDraftData
wizardDocumentTemplateDraftData =
  DocumentTemplateDraftData
    { documentTemplateId = wizardDocumentTemplateDraft.tId
    , questionnaireUuid = Just questionnaire1.uuid
    , knowledgeModelEditorUuid = Nothing
    , formatUuid = Just formatJson.uuid
    , tenantUuid = wizardDocumentTemplateDraft.tenantUuid
    , createdAt = wizardDocumentTemplateDraft.createdAt
    , updatedAt = wizardDocumentTemplateDraft.updatedAt
    }

wizardDocumentTemplateDraftDataEdited :: DocumentTemplateDraftData
wizardDocumentTemplateDraftDataEdited =
  wizardDocumentTemplateDraftData
    { questionnaireUuid = Just questionnaire2.uuid
    , formatUuid = Just formatPdf.uuid
    }

wizardDocumentTemplateDraftCreateDTO :: DocumentTemplateDraftCreateDTO
wizardDocumentTemplateDraftCreateDTO =
  DocumentTemplateDraftCreateDTO
    { name = "New Document Template"
    , templateId = wizardDocumentTemplateNlDraft.templateId
    , version = "3.0.0"
    , basedOn = Just wizardDocumentTemplateDraft.tId
    }

wizardDocumentTemplateDraftChangeDTO :: DocumentTemplateDraftChangeDTO
wizardDocumentTemplateDraftChangeDTO = toChangeDTO wizardDocumentTemplateDraft

wizardDocumentTemplateDraftDataDTO :: DocumentTemplateDraftDataDTO
wizardDocumentTemplateDraftDataDTO =
  DocumentTemplateDraftDataDTO
    { questionnaireUuid = wizardDocumentTemplateDraftDataEdited.questionnaireUuid
    , questionnaire = Just . QuestionnaireMapper.toSuggestion $ questionnaire2
    , knowledgeModelEditorUuid = Nothing
    , knowledgeModelEditor = Nothing
    , formatUuid = wizardDocumentTemplateDraftDataEdited.formatUuid
    }

wizardDocumentTemplateDraftDataChangeDTO :: DocumentTemplateDraftDataChangeDTO
wizardDocumentTemplateDraftDataChangeDTO =
  DocumentTemplateDraftDataChangeDTO
    { questionnaireUuid = wizardDocumentTemplateDraftDataEdited.questionnaireUuid
    , knowledgeModelEditorUuid = Nothing
    , formatUuid = wizardDocumentTemplateDraftDataEdited.formatUuid
    }
