module Wizard.Api.Resource.Questionnaire.QuestionnaireDetailPreviewSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailPreviewJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnairePermSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireSharingSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireVisibilitySM ()
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireDetailPreview
import WizardLib.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateSM ()
import WizardLib.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFormats
import WizardLib.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import qualified WizardLib.DocumentTemplate.Service.DocumentTemplate.DocumentTemplateMapper as DocumentTemplateMapper
import WizardLib.KnowledgeModel.Api.Resource.KnowledgeModel.KnowledgeModelSM ()

instance ToSchema QuestionnaireDetailPreview where
  declareNamedSchema =
    toSwagger $
      QuestionnaireDetailPreview
        { uuid = questionnaire1.uuid
        , name = questionnaire1.name
        , visibility = questionnaire1.visibility
        , sharing = questionnaire1.sharing
        , packageId = questionnaire1.packageId
        , isTemplate = questionnaire1.isTemplate
        , documentTemplateId = Just wizardDocumentTemplate.tId
        , migrationUuid = Nothing
        , permissions = [qtn1AlbertEditQtnPermDto]
        , format = Just . DocumentTemplateMapper.toFormatDTO $ formatJson
        , fileCount = 0
        }
