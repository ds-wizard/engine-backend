module Wizard.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateDrafts where

import Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftChangeDTO
import Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftCreateDTO
import Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftDataChangeDTO
import Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftDataDTO
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import Wizard.Model.DocumentTemplate.DocumentTemplateDraftData
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Service.DocumentTemplate.Draft.DocumentTemplateDraftMapper
import qualified Wizard.Service.Questionnaire.QuestionnaireMapper as QuestionnaireMapper
import WizardLib.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFormats
import WizardLib.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate

wizardDocumentTemplateDraftData :: DocumentTemplateDraftData
wizardDocumentTemplateDraftData =
  DocumentTemplateDraftData
    { documentTemplateId = wizardDocumentTemplateDraft.tId
    , questionnaireUuid = Just questionnaire1.uuid
    , branchUuid = Nothing
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
    , branchUuid = Nothing
    , branch = Nothing
    , formatUuid = wizardDocumentTemplateDraftDataEdited.formatUuid
    }

wizardDocumentTemplateDraftDataChangeDTO :: DocumentTemplateDraftDataChangeDTO
wizardDocumentTemplateDraftDataChangeDTO =
  DocumentTemplateDraftDataChangeDTO
    { questionnaireUuid = wizardDocumentTemplateDraftDataEdited.questionnaireUuid
    , branchUuid = Nothing
    , formatUuid = wizardDocumentTemplateDraftDataEdited.formatUuid
    }
