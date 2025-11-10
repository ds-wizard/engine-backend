module Wizard.Api.Resource.Questionnaire.QuestionnaireDetailSettingsSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Shared.Common.Util.Uuid
import Shared.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateSM ()
import Shared.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFormats
import Shared.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import qualified Shared.DocumentTemplate.Service.DocumentTemplate.DocumentTemplateMapper as DocumentTemplateMapper
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.KnowledgeModelSM ()
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Tags
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Wizard.Api.Resource.DocumentTemplate.DocumentTemplateStateSM ()
import Wizard.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireCommentThreadListSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailSettingsJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnairePermSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireReplySM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireSharingSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireStateSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireVisibilitySM ()
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionListSM ()
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import Wizard.Model.DocumentTemplate.DocumentTemplateState
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireDetailSettings
import qualified Wizard.Service.KnowledgeModel.Package.KnowledgeModelPackageMapper as PackageMapper

instance ToSchema QuestionnaireDetailSettings where
  declareNamedSchema =
    toSwagger $
      QuestionnaireDetailSettings
        { uuid = questionnaire1.uuid
        , name = questionnaire1.name
        , description = questionnaire1.description
        , visibility = questionnaire1.visibility
        , sharing = questionnaire1.sharing
        , selectedQuestionTagUuids = questionnaire1.selectedQuestionTagUuids
        , isTemplate = questionnaire1.isTemplate
        , migrationUuid = Nothing
        , permissions = [qtn1AlbertEditQtnPermDto]
        , projectTags = questionnaire1.projectTags
        , knowledgeModelPackageId = netherlandsKmPackageV2.pId
        , knowledgeModelPackage = PackageMapper.toSimpleDTO netherlandsKmPackageV2
        , knowledgeModelTags = [tagDataScience]
        , documentTemplate = Just $ DocumentTemplateMapper.toDTO wizardDocumentTemplate wizardDocumentTemplateFormats
        , documentTemplateState = Just DefaultDocumentTemplateState
        , documentTemplatePhase = Just DraftDocumentTemplatePhase
        , formatUuid = Just . u' $ "ae3b9e68-e09e-4ad7-b476-67ab5626e873"
        , fileCount = 0
        }
