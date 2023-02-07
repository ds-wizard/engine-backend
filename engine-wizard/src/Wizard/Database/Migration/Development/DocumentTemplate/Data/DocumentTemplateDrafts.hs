module Wizard.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateDrafts where

import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFormats
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import Shared.Model.DocumentTemplate.DocumentTemplate
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
    , formatUuid = Just formatJson.uuid
    , appUuid = wizardDocumentTemplateDraft.appUuid
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
    , formatUuid = wizardDocumentTemplateDraftDataEdited.formatUuid
    , questionnaire = Just . QuestionnaireMapper.toSuggestion $ questionnaire2
    }

wizardDocumentTemplateDraftDataChangeDTO :: DocumentTemplateDraftDataChangeDTO
wizardDocumentTemplateDraftDataChangeDTO =
  DocumentTemplateDraftDataChangeDTO
    { questionnaireUuid = wizardDocumentTemplateDraftDataEdited.questionnaireUuid
    , formatUuid = wizardDocumentTemplateDraftDataEdited.formatUuid
    }
