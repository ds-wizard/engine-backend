module Wizard.Database.Migration.Development.Questionnaire.Data.MigratorStates where

import qualified Data.Map.Strict as M

import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Questions
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Wizard.Api.Resource.Questionnaire.Migration.MigratorStateChangeDTO
import Wizard.Api.Resource.Questionnaire.Migration.MigratorStateCreateDTO
import Wizard.Api.Resource.Questionnaire.Migration.MigratorStateDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailQuestionnaireDTO
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import Wizard.Database.Migration.Development.Tenant.Data.Tenants
import Wizard.Model.Questionnaire.MigratorState
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireContent
import Wizard.Model.Tenant.Tenant
import Wizard.Service.Questionnaire.QuestionnaireMapper

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
          questionnaire4Ctn.phaseUuid
          questionnaire4Ctn.replies
          questionnaire4Ctn.labels
    , newQuestionnaire =
        toDetailQuestionnaireDTO
          (toDetailQuestionnaire questionnaire4Upgraded Nothing [] 0 0)
          M.empty
          M.empty
          km1NetherlandsV2
          questionnaire4Ctn.phaseUuid
          questionnaire4Ctn.replies
          questionnaire4Ctn.labels
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
          questionnaire4Ctn.phaseUuid
          questionnaire4Ctn.replies
          questionnaire4Ctn.labels
    , newQuestionnaire =
        toDetailQuestionnaireDTO
          (toDetailQuestionnaire questionnaire4VisibleViewUpgraded Nothing [] 0 0)
          M.empty
          M.empty
          km1NetherlandsV2
          questionnaire4Ctn.phaseUuid
          questionnaire4Ctn.replies
          questionnaire4Ctn.labels
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
          questionnaire4Ctn.phaseUuid
          questionnaire4Ctn.replies
          questionnaire4Ctn.labels
    , newQuestionnaire =
        toDetailQuestionnaireDTO
          (toDetailQuestionnaire questionnaire4VisibleEditUpgraded Nothing [] 0 0)
          M.empty
          M.empty
          km1NetherlandsV2
          questionnaire4Ctn.phaseUuid
          questionnaire4Ctn.replies
          questionnaire4Ctn.labels
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
    { targetKnowledgeModelPackageId = netherlandsKmPackageV2.pId
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
