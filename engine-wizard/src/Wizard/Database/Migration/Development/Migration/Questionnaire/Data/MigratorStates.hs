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
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.Migration.Questionnaire.MigratorState
import Wizard.Model.Questionnaire.QuestionnaireState
import Wizard.Service.Questionnaire.Event.QuestionnaireEventMapper
import Wizard.Service.Questionnaire.QuestionnaireMapper
import Wizard.Service.Questionnaire.Version.QuestionnaireVersionMapper

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
          questionnaire4Ctn
          (PM.toPackage netherlandsPackage)
          ["1.0.0", "2.0.0"]
          km1Netherlands
          QSOutdated
          (Just commonWizardTemplate)
          (Just templateFormatJson)
          (questionnaire4Ctn ^. replies)
          []
          (fmap (\event -> toEventDTO event (Just userAlbert)) (questionnaire4 ^. events))
          (fmap (`toVersionDTO` userAlbert) (questionnaire4 ^. versions))
    , _migratorStateDTONewQuestionnaire =
        toDetailWithPackageWithEventsDTO
          questionnaire4Upgraded
          questionnaire4Ctn
          (PM.toPackage netherlandsPackageV2)
          ["1.0.0", "2.0.0"]
          km1NetherlandsV2
          QSMigrating
          (Just commonWizardTemplate)
          (Just templateFormatJson)
          (questionnaire4Ctn ^. replies)
          []
          (fmap (\event -> toEventDTO event (Just userAlbert)) (questionnaire4Upgraded ^. events))
          (fmap (`toVersionDTO` userAlbert) (questionnaire4Upgraded ^. versions))
    , _migratorStateDTOResolvedQuestionUuids = [question2 ^. uuid]
    }

nlQtnMigrationStateVisibleViewDto :: MigratorStateDTO
nlQtnMigrationStateVisibleViewDto =
  MigratorStateDTO
    { _migratorStateDTOOldQuestionnaire =
        toDetailWithPackageWithEventsDTO
          questionnaire4VisibleView
          questionnaire4Ctn
          (PM.toPackage netherlandsPackage)
          ["1.0.0", "2.0.0"]
          km1Netherlands
          QSOutdated
          (Just commonWizardTemplate)
          (Just templateFormatJson)
          (questionnaire4Ctn ^. replies)
          []
          (fmap (\event -> toEventDTO event (Just userAlbert)) (questionnaire4VisibleView ^. events))
          (fmap (`toVersionDTO` userAlbert) (questionnaire4VisibleView ^. versions))
    , _migratorStateDTONewQuestionnaire =
        toDetailWithPackageWithEventsDTO
          questionnaire4VisibleViewUpgraded
          questionnaire4Ctn
          (PM.toPackage netherlandsPackageV2)
          ["1.0.0", "2.0.0"]
          km1NetherlandsV2
          QSMigrating
          (Just commonWizardTemplate)
          (Just templateFormatJson)
          (questionnaire4Ctn ^. replies)
          []
          (fmap (\event -> toEventDTO event (Just userAlbert)) (questionnaire4VisibleViewUpgraded ^. events))
          (fmap (`toVersionDTO` userAlbert) (questionnaire4VisibleViewUpgraded ^. versions))
    , _migratorStateDTOResolvedQuestionUuids = nlQtnMigrationStateDto ^. resolvedQuestionUuids
    }

nlQtnMigrationStateVisibleEditDto :: MigratorStateDTO
nlQtnMigrationStateVisibleEditDto =
  MigratorStateDTO
    { _migratorStateDTOOldQuestionnaire =
        toDetailWithPackageWithEventsDTO
          questionnaire4VisibleEdit
          questionnaire4Ctn
          (PM.toPackage netherlandsPackage)
          ["1.0.0", "2.0.0"]
          km1Netherlands
          QSOutdated
          (Just commonWizardTemplate)
          (Just templateFormatJson)
          (questionnaire4Ctn ^. replies)
          []
          (fmap (\event -> toEventDTO event (Just userAlbert)) (questionnaire4VisibleEdit ^. events))
          (fmap (`toVersionDTO` userAlbert) (questionnaire4VisibleEdit ^. versions))
    , _migratorStateDTONewQuestionnaire =
        toDetailWithPackageWithEventsDTO
          questionnaire4VisibleEditUpgraded
          questionnaire4Ctn
          (PM.toPackage netherlandsPackageV2)
          ["1.0.0", "2.0.0"]
          km1NetherlandsV2
          QSMigrating
          (Just commonWizardTemplate)
          (Just templateFormatJson)
          (questionnaire4Ctn ^. replies)
          []
          (fmap (\event -> toEventDTO event (Just userAlbert)) (questionnaire4VisibleEditUpgraded ^. events))
          (fmap (`toVersionDTO` userAlbert) (questionnaire4VisibleEditUpgraded ^. versions))
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
