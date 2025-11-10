module Wizard.Api.Resource.Questionnaire.QuestionnaireDetailPreviewSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Shared.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateSM ()
import Shared.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFormats
import Shared.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import qualified Shared.DocumentTemplate.Service.DocumentTemplate.DocumentTemplateMapper as DocumentTemplateMapper
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.KnowledgeModelSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailPreviewJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnairePermSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireSharingSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireVisibilitySM ()
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireDetailPreview

instance ToSchema QuestionnaireDetailPreview where
  declareNamedSchema =
    toSwagger $
      QuestionnaireDetailPreview
        { uuid = questionnaire1.uuid
        , name = questionnaire1.name
        , visibility = questionnaire1.visibility
        , sharing = questionnaire1.sharing
        , knowledgeModelPackageId = questionnaire1.knowledgeModelPackageId
        , isTemplate = questionnaire1.isTemplate
        , documentTemplateId = Just wizardDocumentTemplate.tId
        , migrationUuid = Nothing
        , permissions = [qtn1AlbertEditQtnPermDto]
        , format = Just . DocumentTemplateMapper.toFormatSimple $ formatJson
        , fileCount = 0
        }
