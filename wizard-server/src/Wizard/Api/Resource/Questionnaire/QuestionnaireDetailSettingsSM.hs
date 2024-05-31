module Wizard.Api.Resource.Questionnaire.QuestionnaireDetailSettingsSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Shared.Common.Util.Uuid
import Wizard.Api.Resource.DocumentTemplate.DocumentTemplateStateSM ()
import Wizard.Api.Resource.Package.PackageSimpleSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireCommentThreadListSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailSettingsJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnairePermSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireReplySM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireSharingSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireStateSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireVisibilitySM ()
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionSM ()
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import Wizard.Model.DocumentTemplate.DocumentTemplateState
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireDetailSettings
import qualified Wizard.Service.Package.PackageMapper as PackageMapper
import WizardLib.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateSM ()
import WizardLib.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import qualified WizardLib.DocumentTemplate.Service.DocumentTemplate.DocumentTemplateMapper as DocumentTemplateMapper
import WizardLib.KnowledgeModel.Api.Resource.KnowledgeModel.KnowledgeModelSM ()
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Tags
import WizardLib.KnowledgeModel.Database.Migration.Development.Package.Data.Packages
import WizardLib.KnowledgeModel.Model.Package.PackageWithEvents
import qualified WizardLib.KnowledgeModel.Service.Package.PackageMapper as PackageMapper

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
        , packageId = netherlandsPackageV2.pId
        , package = PackageMapper.toSimpleDTO . PackageMapper.toPackage $ netherlandsPackageV2
        , knowledgeModelTags = [tagDataScience]
        , documentTemplate = Just . DocumentTemplateMapper.toDTO $ wizardDocumentTemplate
        , documentTemplateState = Just DefaultDocumentTemplateState
        , documentTemplatePhase = Just DraftDocumentTemplatePhase
        , formatUuid = Just . u' $ "ae3b9e68-e09e-4ad7-b476-67ab5626e873"
        }
