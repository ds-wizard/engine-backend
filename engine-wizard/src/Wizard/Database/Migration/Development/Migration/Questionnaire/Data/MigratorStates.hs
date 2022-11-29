module Wizard.Database.Migration.Development.Migration.Questionnaire.Data.MigratorStates where

import qualified Data.Map.Strict as M

import Shared.Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
import Shared.Database.Migration.Development.KnowledgeModel.Data.Questions
import Shared.Database.Migration.Development.Package.Data.Packages
import Shared.Database.Migration.Development.Template.Data.Templates
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.Package.PackageWithEvents
import qualified Shared.Service.Package.PackageMapper as PM
import Wizard.Api.Resource.Migration.Questionnaire.MigratorStateChangeDTO
import Wizard.Api.Resource.Migration.Questionnaire.MigratorStateCreateDTO
import Wizard.Api.Resource.Migration.Questionnaire.MigratorStateDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailDTO
import Wizard.Database.Migration.Development.App.Data.Apps
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.App.App
import Wizard.Model.Migration.Questionnaire.MigratorState
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireContent
import Wizard.Model.Questionnaire.QuestionnaireState
import Wizard.Service.Questionnaire.QuestionnaireMapper
import Wizard.Service.Questionnaire.Version.QuestionnaireVersionMapper

nlQtnMigrationState :: MigratorState
nlQtnMigrationState =
  MigratorState
    { oldQuestionnaireUuid = nlQtnMigrationStateDto.oldQuestionnaire.uuid
    , newQuestionnaireUuid = nlQtnMigrationStateDto.newQuestionnaire.uuid
    , resolvedQuestionUuids = [question2.uuid]
    , appUuid = defaultApp.uuid
    }

nlQtnMigrationStateDto :: MigratorStateDTO
nlQtnMigrationStateDto =
  MigratorStateDTO
    { oldQuestionnaire =
        toDetailWithPackageWithEventsDTO
          questionnaire4
          questionnaire4Ctn
          (PM.toPackage netherlandsPackage)
          ["1.0.0", "2.0.0"]
          km1Netherlands
          QSOutdated
          (Just commonWizardTemplate)
          (Just templateFormatJson)
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
          ["1.0.0", "2.0.0"]
          km1NetherlandsV2
          QSMigrating
          (Just commonWizardTemplate)
          (Just templateFormatJson)
          questionnaire4Ctn.replies
          M.empty
          []
          (fmap (`toVersionDTO` Just userAlbert) questionnaire4Upgraded.versions)
          Nothing
    , resolvedQuestionUuids = [question2.uuid]
    , appUuid = defaultApp.uuid
    }

nlQtnMigrationStateVisibleViewDto :: MigratorStateDTO
nlQtnMigrationStateVisibleViewDto =
  MigratorStateDTO
    { oldQuestionnaire =
        toDetailWithPackageWithEventsDTO
          questionnaire4VisibleView
          questionnaire4Ctn
          (PM.toPackage netherlandsPackage)
          ["1.0.0", "2.0.0"]
          km1Netherlands
          QSOutdated
          (Just commonWizardTemplate)
          (Just templateFormatJson)
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
          ["1.0.0", "2.0.0"]
          km1NetherlandsV2
          QSMigrating
          (Just commonWizardTemplate)
          (Just templateFormatJson)
          questionnaire4Ctn.replies
          M.empty
          []
          (fmap (`toVersionDTO` Just userAlbert) questionnaire4VisibleViewUpgraded.versions)
          Nothing
    , resolvedQuestionUuids = nlQtnMigrationStateDto.resolvedQuestionUuids
    , appUuid = defaultApp.uuid
    }

nlQtnMigrationStateVisibleEditDto :: MigratorStateDTO
nlQtnMigrationStateVisibleEditDto =
  MigratorStateDTO
    { oldQuestionnaire =
        toDetailWithPackageWithEventsDTO
          questionnaire4VisibleEdit
          questionnaire4Ctn
          (PM.toPackage netherlandsPackage)
          ["1.0.0", "2.0.0"]
          km1Netherlands
          QSOutdated
          (Just commonWizardTemplate)
          (Just templateFormatJson)
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
          ["1.0.0", "2.0.0"]
          km1NetherlandsV2
          QSMigrating
          (Just commonWizardTemplate)
          (Just templateFormatJson)
          questionnaire4Ctn.replies
          M.empty
          []
          (fmap (`toVersionDTO` Just userAlbert) questionnaire4VisibleEditUpgraded.versions)
          Nothing
    , resolvedQuestionUuids = nlQtnMigrationStateDto.resolvedQuestionUuids
    , appUuid = defaultApp.uuid
    }

nlQtnMigrationStateDtoEdited :: MigratorStateDTO
nlQtnMigrationStateDtoEdited =
  MigratorStateDTO
    { oldQuestionnaire = nlQtnMigrationStateDto.oldQuestionnaire
    , newQuestionnaire = nlQtnMigrationStateDto.newQuestionnaire
    , resolvedQuestionUuids = [question2.uuid, question3.uuid]
    , appUuid = nlQtnMigrationStateDto.appUuid
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
    , appUuid = differentApp.uuid
    }
