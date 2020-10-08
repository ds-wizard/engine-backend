module Wizard.Database.Migration.Development.Migration.Questionnaire.Data.MigratorStates where

import Control.Lens ((^.))

import LensesConfig
import Shared.Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
import Shared.Database.Migration.Development.KnowledgeModel.Data.Questions
import Shared.Database.Migration.Development.Package.Data.Packages
import Shared.Database.Migration.Development.Template.Data.Templates
import qualified Shared.Service.Package.PackageMapper as PM
import Wizard.Api.Resource.Migration.Questionnaire.MigratorStateChangeDTO
import Wizard.Api.Resource.Migration.Questionnaire.MigratorStateCreateDTO
import Wizard.Api.Resource.Migration.Questionnaire.MigratorStateDTO
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import Wizard.Model.Migration.Questionnaire.MigratorState
import Wizard.Model.Questionnaire.QuestionnaireState
import Wizard.Service.Questionnaire.QuestionnaireMapper

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
        toDetailWithPackageWithEventsDTO
          questionnaire4
          (PM.toPackage netherlandsPackage)
          km1Netherlands
          QSOutdated
          (Just commonWizardTemplate)
          (Just templateFormatJson)
    , _migratorStateDTONewQuestionnaire =
        toDetailWithPackageWithEventsDTO
          questionnaire4Upgraded
          (PM.toPackage netherlandsPackageV2)
          km1NetherlandsV2
          QSMigrating
          (Just commonWizardTemplate)
          (Just templateFormatJson)
    , _migratorStateDTOResolvedQuestionUuids = [question2 ^. uuid]
    }

nlQtnMigrationStateVisibleViewDto :: MigratorStateDTO
nlQtnMigrationStateVisibleViewDto =
  MigratorStateDTO
    { _migratorStateDTOOldQuestionnaire =
        toDetailWithPackageWithEventsDTO
          questionnaire4VisibleView
          (PM.toPackage netherlandsPackage)
          km1Netherlands
          QSOutdated
          (Just commonWizardTemplate)
          (Just templateFormatJson)
    , _migratorStateDTONewQuestionnaire =
        toDetailWithPackageWithEventsDTO
          questionnaire4VisibleViewUpgraded
          (PM.toPackage netherlandsPackageV2)
          km1NetherlandsV2
          QSMigrating
          (Just commonWizardTemplate)
          (Just templateFormatJson)
    , _migratorStateDTOResolvedQuestionUuids = nlQtnMigrationStateDto ^. resolvedQuestionUuids
    }

nlQtnMigrationStateVisibleEditDto :: MigratorStateDTO
nlQtnMigrationStateVisibleEditDto =
  MigratorStateDTO
    { _migratorStateDTOOldQuestionnaire =
        toDetailWithPackageWithEventsDTO
          questionnaire4VisibleEdit
          (PM.toPackage netherlandsPackage)
          km1Netherlands
          QSOutdated
          (Just commonWizardTemplate)
          (Just templateFormatJson)
    , _migratorStateDTONewQuestionnaire =
        toDetailWithPackageWithEventsDTO
          questionnaire4VisibleEditUpgraded
          (PM.toPackage netherlandsPackageV2)
          km1NetherlandsV2
          QSMigrating
          (Just commonWizardTemplate)
          (Just templateFormatJson)
    , _migratorStateDTOResolvedQuestionUuids = nlQtnMigrationStateDto ^. resolvedQuestionUuids
    }

nlQtnMigrationStateDtoEdited :: MigratorStateDTO
nlQtnMigrationStateDtoEdited =
  MigratorStateDTO
    { _migratorStateDTOOldQuestionnaire = nlQtnMigrationStateDto ^. oldQuestionnaire
    , _migratorStateDTONewQuestionnaire = nlQtnMigrationStateDto ^. newQuestionnaire
    , _migratorStateDTOResolvedQuestionUuids = [question2 ^. uuid, question3 ^. uuid]
    }

migratorStateCreate :: MigratorStateCreateDTO
migratorStateCreate =
  MigratorStateCreateDTO
    { _migratorStateCreateDTOTargetPackageId = netherlandsPackageV2 ^. pId
    , _migratorStateCreateDTOTargetTagUuids = questionnaire4Upgraded ^. selectedTagUuids
    }

migratorStateChange :: MigratorStateChangeDTO
migratorStateChange =
  MigratorStateChangeDTO
    {_migratorStateChangeDTOResolvedQuestionUuids = nlQtnMigrationStateDtoEdited ^. resolvedQuestionUuids}
