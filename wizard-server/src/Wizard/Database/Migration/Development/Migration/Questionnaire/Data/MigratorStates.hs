module Wizard.Database.Migration.Development.Migration.Questionnaire.Data.MigratorStates where

import qualified Data.Map.Strict as M

import Wizard.Api.Resource.Migration.Questionnaire.MigratorStateChangeDTO
import Wizard.Api.Resource.Migration.Questionnaire.MigratorStateCreateDTO
import Wizard.Api.Resource.Migration.Questionnaire.MigratorStateDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailDTO
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import Wizard.Database.Migration.Development.Tenant.Data.Tenants
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.Migration.Questionnaire.MigratorState
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireContent
import Wizard.Model.Questionnaire.QuestionnaireState
import Wizard.Model.Tenant.Tenant
import Wizard.Service.Questionnaire.QuestionnaireMapper
import Wizard.Service.Questionnaire.Version.QuestionnaireVersionMapper
import WizardLib.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFormats
import WizardLib.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Questions
import WizardLib.KnowledgeModel.Database.Migration.Development.Package.Data.Packages
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import WizardLib.KnowledgeModel.Model.Package.PackageWithEvents
import qualified WizardLib.KnowledgeModel.Service.Package.PackageMapper as PM

nlQtnMigrationState :: MigratorState
nlQtnMigrationState =
  MigratorState
    { oldQuestionnaireUuid = nlQtnMigrationStateDto.oldQuestionnaire.uuid
    , newQuestionnaireUuid = nlQtnMigrationStateDto.newQuestionnaire.uuid
    , resolvedQuestionUuids = [question2.uuid]
    , tenantUuid = defaultTenant.uuid
    }

nlQtnMigrationStateDto :: MigratorStateDTO
nlQtnMigrationStateDto =
  MigratorStateDTO
    { oldQuestionnaire =
        toDetailWithPackageWithEventsDTO
          questionnaire4
          questionnaire4Ctn
          (PM.toPackage netherlandsPackage)
          km1Netherlands
          QSOutdated
          (Just wizardDocumentTemplate)
          (Just formatJson)
          questionnaire4Ctn.replies
          M.empty
          []
          (fmap (`toVersionDTO` Just userAlbert) questionnaire4.versions)
          (Just questionnaire4Upgraded.uuid)
    , newQuestionnaire =
        toDetailWithPackageWithEventsDTO
          questionnaire4Upgraded
          questionnaire4Ctn
          (PM.toPackage netherlandsPackageV2)
          km1NetherlandsV2
          QSMigrating
          (Just wizardDocumentTemplate)
          (Just formatJson)
          questionnaire4Ctn.replies
          M.empty
          []
          (fmap (`toVersionDTO` Just userAlbert) questionnaire4Upgraded.versions)
          Nothing
    , resolvedQuestionUuids = [question2.uuid]
    , tenantUuid = defaultTenant.uuid
    }

nlQtnMigrationStateVisibleViewDto :: MigratorStateDTO
nlQtnMigrationStateVisibleViewDto =
  MigratorStateDTO
    { oldQuestionnaire =
        toDetailWithPackageWithEventsDTO
          questionnaire4VisibleView
          questionnaire4Ctn
          (PM.toPackage netherlandsPackage)
          km1Netherlands
          QSOutdated
          (Just wizardDocumentTemplate)
          (Just formatJson)
          questionnaire4Ctn.replies
          M.empty
          []
          (fmap (`toVersionDTO` Just userAlbert) questionnaire4VisibleView.versions)
          (Just questionnaire4Upgraded.uuid)
    , newQuestionnaire =
        toDetailWithPackageWithEventsDTO
          questionnaire4VisibleViewUpgraded
          questionnaire4Ctn
          (PM.toPackage netherlandsPackageV2)
          km1NetherlandsV2
          QSMigrating
          (Just wizardDocumentTemplate)
          (Just formatJson)
          questionnaire4Ctn.replies
          M.empty
          []
          (fmap (`toVersionDTO` Just userAlbert) questionnaire4VisibleViewUpgraded.versions)
          Nothing
    , resolvedQuestionUuids = nlQtnMigrationStateDto.resolvedQuestionUuids
    , tenantUuid = defaultTenant.uuid
    }

nlQtnMigrationStateVisibleEditDto :: MigratorStateDTO
nlQtnMigrationStateVisibleEditDto =
  MigratorStateDTO
    { oldQuestionnaire =
        toDetailWithPackageWithEventsDTO
          questionnaire4VisibleEdit
          questionnaire4Ctn
          (PM.toPackage netherlandsPackage)
          km1Netherlands
          QSOutdated
          (Just wizardDocumentTemplate)
          (Just formatJson)
          questionnaire4Ctn.replies
          M.empty
          []
          (fmap (`toVersionDTO` Just userAlbert) questionnaire4VisibleEdit.versions)
          (Just questionnaire4Upgraded.uuid)
    , newQuestionnaire =
        toDetailWithPackageWithEventsDTO
          questionnaire4VisibleEditUpgraded
          questionnaire4Ctn
          (PM.toPackage netherlandsPackageV2)
          km1NetherlandsV2
          QSMigrating
          (Just wizardDocumentTemplate)
          (Just formatJson)
          questionnaire4Ctn.replies
          M.empty
          []
          (fmap (`toVersionDTO` Just userAlbert) questionnaire4VisibleEditUpgraded.versions)
          Nothing
    , resolvedQuestionUuids = nlQtnMigrationStateDto.resolvedQuestionUuids
    , tenantUuid = defaultTenant.uuid
    }

nlQtnMigrationStateDtoEdited :: MigratorStateDTO
nlQtnMigrationStateDtoEdited =
  MigratorStateDTO
    { oldQuestionnaire = nlQtnMigrationStateDto.oldQuestionnaire
    , newQuestionnaire = nlQtnMigrationStateDto.newQuestionnaire
    , resolvedQuestionUuids = [question2.uuid, question3.uuid]
    , tenantUuid = nlQtnMigrationStateDto.tenantUuid
    }

migratorStateCreate :: MigratorStateCreateDTO
migratorStateCreate =
  MigratorStateCreateDTO
    { targetPackageId = netherlandsPackageV2.pId
    , targetTagUuids = questionnaire4Upgraded.selectedQuestionTagUuids
    }

migratorStateChange :: MigratorStateChangeDTO
migratorStateChange =
  MigratorStateChangeDTO
    { resolvedQuestionUuids = nlQtnMigrationStateDtoEdited.resolvedQuestionUuids
    }

differentQtnMigrationState :: MigratorState
differentQtnMigrationState =
  MigratorState
    { oldQuestionnaireUuid = differentQuestionnaire.uuid
    , newQuestionnaireUuid = differentQuestionnaire.uuid
    , resolvedQuestionUuids = [question2.uuid]
    , tenantUuid = differentTenant.uuid
    }
