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
import Wizard.Database.Migration.Development.App.Data.Apps
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
    , _migratorStateAppUuid = defaultApp ^. uuid
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
          (questionnaire4Ctn ^. commentThreadsMap)
          []
          (fmap (\event -> toEventDTO event (Just userAlbert)) (questionnaire4 ^. events))
          (fmap (`toVersionDTO` Just userAlbert) (questionnaire4 ^. versions))
          (Just $ questionnaire4Upgraded ^. uuid)
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
          (questionnaire4Ctn ^. commentThreadsMap)
          []
          (fmap (\event -> toEventDTO event (Just userAlbert)) (questionnaire4Upgraded ^. events))
          (fmap (`toVersionDTO` Just userAlbert) (questionnaire4Upgraded ^. versions))
          Nothing
    , _migratorStateDTOResolvedQuestionUuids = [question2 ^. uuid]
    , _migratorStateDTOAppUuid = defaultApp ^. uuid
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
          (questionnaire4Ctn ^. commentThreadsMap)
          []
          (fmap (\event -> toEventDTO event (Just userAlbert)) (questionnaire4VisibleView ^. events))
          (fmap (`toVersionDTO` Just userAlbert) (questionnaire4VisibleView ^. versions))
          (Just $ questionnaire4Upgraded ^. uuid)
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
          (questionnaire4Ctn ^. commentThreadsMap)
          []
          (fmap (\event -> toEventDTO event (Just userAlbert)) (questionnaire4VisibleViewUpgraded ^. events))
          (fmap (`toVersionDTO` Just userAlbert) (questionnaire4VisibleViewUpgraded ^. versions))
          Nothing
    , _migratorStateDTOResolvedQuestionUuids = nlQtnMigrationStateDto ^. resolvedQuestionUuids
    , _migratorStateDTOAppUuid = defaultApp ^. uuid
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
          (questionnaire4Ctn ^. commentThreadsMap)
          []
          (fmap (\event -> toEventDTO event (Just userAlbert)) (questionnaire4VisibleEdit ^. events))
          (fmap (`toVersionDTO` Just userAlbert) (questionnaire4VisibleEdit ^. versions))
          (Just $ questionnaire4Upgraded ^. uuid)
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
          (questionnaire4Ctn ^. commentThreadsMap)
          []
          (fmap (\event -> toEventDTO event (Just userAlbert)) (questionnaire4VisibleEditUpgraded ^. events))
          (fmap (`toVersionDTO` Just userAlbert) (questionnaire4VisibleEditUpgraded ^. versions))
          Nothing
    , _migratorStateDTOResolvedQuestionUuids = nlQtnMigrationStateDto ^. resolvedQuestionUuids
    , _migratorStateDTOAppUuid = defaultApp ^. uuid
    }

nlQtnMigrationStateDtoEdited :: MigratorStateDTO
nlQtnMigrationStateDtoEdited =
  MigratorStateDTO
    { _migratorStateDTOOldQuestionnaire = nlQtnMigrationStateDto ^. oldQuestionnaire
    , _migratorStateDTONewQuestionnaire = nlQtnMigrationStateDto ^. newQuestionnaire
    , _migratorStateDTOResolvedQuestionUuids = [question2 ^. uuid, question3 ^. uuid]
    , _migratorStateDTOAppUuid = nlQtnMigrationStateDto ^. appUuid
    }

migratorStateCreate :: MigratorStateCreateDTO
migratorStateCreate =
  MigratorStateCreateDTO
    { _migratorStateCreateDTOTargetPackageId = netherlandsPackageV2 ^. pId
    , _migratorStateCreateDTOTargetTagUuids = questionnaire4Upgraded ^. selectedQuestionTagUuids
    }

migratorStateChange :: MigratorStateChangeDTO
migratorStateChange =
  MigratorStateChangeDTO
    {_migratorStateChangeDTOResolvedQuestionUuids = nlQtnMigrationStateDtoEdited ^. resolvedQuestionUuids}

differentQtnMigrationState :: MigratorState
differentQtnMigrationState =
  MigratorState
    { _migratorStateOldQuestionnaireUuid = differentQuestionnaire ^. uuid
    , _migratorStateNewQuestionnaireUuid = differentQuestionnaire ^. uuid
    , _migratorStateResolvedQuestionUuids = [question2 ^. uuid]
    , _migratorStateAppUuid = differentApp ^. uuid
    }
