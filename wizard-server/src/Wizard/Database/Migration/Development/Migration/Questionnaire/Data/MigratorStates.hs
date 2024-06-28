module Wizard.Database.Migration.Development.Migration.Questionnaire.Data.MigratorStates where

import qualified Data.Map.Strict as M

import Wizard.Api.Resource.Migration.Questionnaire.MigratorStateChangeDTO
import Wizard.Api.Resource.Migration.Questionnaire.MigratorStateCreateDTO
import Wizard.Api.Resource.Migration.Questionnaire.MigratorStateDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailQuestionnaireDTO
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import Wizard.Database.Migration.Development.Tenant.Data.Tenants
import Wizard.Model.Migration.Questionnaire.MigratorState
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Tenant.Tenant
import Wizard.Service.Questionnaire.QuestionnaireMapper
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Questions
import WizardLib.KnowledgeModel.Database.Migration.Development.Package.Data.Packages
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import WizardLib.KnowledgeModel.Model.Package.PackageWithEvents

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
        toDetailQuestionnaireDTO
          (toDetailQuestionnaire questionnaire4 (Just questionnaire4Upgraded.uuid) [] 0 0)
          M.empty
          M.empty
          km1Netherlands
          questionnaire4Ctn
    , newQuestionnaire =
        toDetailQuestionnaireDTO
          (toDetailQuestionnaire questionnaire4Upgraded Nothing [] 0 0)
          M.empty
          M.empty
          km1NetherlandsV2
          questionnaire4Ctn
    , resolvedQuestionUuids = [question2.uuid]
    , tenantUuid = defaultTenant.uuid
    }

nlQtnMigrationStateVisibleViewDto :: MigratorStateDTO
nlQtnMigrationStateVisibleViewDto =
  MigratorStateDTO
    { oldQuestionnaire =
        toDetailQuestionnaireDTO
          (toDetailQuestionnaire questionnaire4VisibleView (Just questionnaire4Upgraded.uuid) [] 0 0)
          M.empty
          M.empty
          km1Netherlands
          questionnaire4Ctn
    , newQuestionnaire =
        toDetailQuestionnaireDTO
          (toDetailQuestionnaire questionnaire4VisibleViewUpgraded Nothing [] 0 0)
          M.empty
          M.empty
          km1NetherlandsV2
          questionnaire4Ctn
    , resolvedQuestionUuids = nlQtnMigrationStateDto.resolvedQuestionUuids
    , tenantUuid = defaultTenant.uuid
    }

nlQtnMigrationStateVisibleEditDto :: MigratorStateDTO
nlQtnMigrationStateVisibleEditDto =
  MigratorStateDTO
    { oldQuestionnaire =
        toDetailQuestionnaireDTO
          (toDetailQuestionnaire questionnaire4VisibleEdit (Just questionnaire4Upgraded.uuid) [] 0 0)
          M.empty
          M.empty
          km1Netherlands
          questionnaire4Ctn
    , newQuestionnaire =
        toDetailQuestionnaireDTO
          (toDetailQuestionnaire questionnaire4VisibleEditUpgraded Nothing [] 0 0)
          M.empty
          M.empty
          km1NetherlandsV2
          questionnaire4Ctn
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
