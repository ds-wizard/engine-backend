module Database.Migration.Development.Migration.Questionnaire.Data.MigratorStates where

import Control.Lens ((^.))

import Api.Resource.Migration.Questionnaire.MigratorStateDTO
import Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
import Database.Migration.Development.KnowledgeModel.Data.Questions
import Database.Migration.Development.Package.Data.Packages
import Database.Migration.Development.Questionnaire.Data.Questionnaires
import LensesConfig
import Model.Migration.Questionnaire.MigratorState
import Model.Questionnaire.QuestionnaireState
import Service.Questionnaire.QuestionnaireMapper

nlQtnMigrationState :: MigratorState
nlQtnMigrationState =
  MigratorState
  { _migratorStateOldQuestionnaireUuid = nlQtnMigrationStateDto ^. oldQuestionnaire . uuid
  , _migratorStateNewQuestionnaireUuid = nlQtnMigrationStateDto ^. newQuestionnaire . uuid
  , _migratorStateResolvedQuestionUuids = [question2 ^. uuid]
  }

nlQtnMigrationStateDto :: MigratorStateDTO
nlQtnMigrationStateDto =
  MigratorStateDTO
  { _migratorStateDTOOldQuestionnaire =
      toDetailWithPackageWithEventsDTO questionnaire4 netherlandsPackage km1Netherlands QSOutdated
  , _migratorStateDTONewQuestionnaire =
      toDetailWithPackageWithEventsDTO questionnaire4Upgraded netherlandsPackageV2 km1NetherlandsV2 QSMigrating
  , _migratorStateDTOResolvedQuestionUuids = [question2 ^. uuid]
  }

nlQtnMigrationStatePublicReadOnlyDto :: MigratorStateDTO
nlQtnMigrationStatePublicReadOnlyDto =
  MigratorStateDTO
  { _migratorStateDTOOldQuestionnaire =
      toDetailWithPackageWithEventsDTO questionnaire4PublicReadOnly netherlandsPackage km1Netherlands QSOutdated
  , _migratorStateDTONewQuestionnaire =
      toDetailWithPackageWithEventsDTO
        questionnaire4PublicReadOnlyUpgraded
        netherlandsPackageV2
        km1NetherlandsV2
        QSMigrating
  , _migratorStateDTOResolvedQuestionUuids = nlQtnMigrationStateDto ^. resolvedQuestionUuids
  }

nlQtnMigrationStatePublicDto :: MigratorStateDTO
nlQtnMigrationStatePublicDto =
  MigratorStateDTO
  { _migratorStateDTOOldQuestionnaire =
      toDetailWithPackageWithEventsDTO questionnaire4Public netherlandsPackage km1Netherlands QSOutdated
  , _migratorStateDTONewQuestionnaire =
      toDetailWithPackageWithEventsDTO questionnaire4PublicUpgraded netherlandsPackageV2 km1NetherlandsV2 QSMigrating
  , _migratorStateDTOResolvedQuestionUuids = nlQtnMigrationStateDto ^. resolvedQuestionUuids
  }

nlQtnMigrationStateDtoEdited :: MigratorStateDTO
nlQtnMigrationStateDtoEdited =
  MigratorStateDTO
  { _migratorStateDTOOldQuestionnaire = nlQtnMigrationStateDto ^. oldQuestionnaire
  , _migratorStateDTONewQuestionnaire = nlQtnMigrationStateDto ^. newQuestionnaire
  , _migratorStateDTOResolvedQuestionUuids = [question2 ^. uuid, question3 ^. uuid]
  }
