module Wizard.Service.Dev.DevOperationDefinitions where

import Control.Monad.Reader (ask, liftIO)
import Data.Foldable (traverse_)

import Wizard.Api.Resource.Dev.DevExecutionDTO
import Wizard.Database.DAO.App.AppDAO
import Wizard.Database.DAO.PersistentCommand.PersistentCommandDAO
import Wizard.Model.App.App
import Wizard.Model.Context.AppContext
import Wizard.Model.Dev.Dev
import Wizard.Service.ActionKey.ActionKeyService
import Wizard.Service.Branch.Event.BranchEventService
import Wizard.Service.Cache.CacheService
import Wizard.Service.Config.App.AppConfigCommandExecutor
import Wizard.Service.Config.App.AppConfigService
import Wizard.Service.Feedback.FeedbackService
import Wizard.Service.PersistentCommand.PersistentCommandService
import Wizard.Service.Questionnaire.Event.QuestionnaireEventService
import Wizard.Service.Questionnaire.QuestionnaireService
import Wizard.Service.Registry.RegistryService
import Wizard.Service.UserToken.UserTokenService
import Wizard.Util.Context

-- ---------------------------------------------------------------------------------------------------------------------
-- ACTION KEY
-- ---------------------------------------------------------------------------------------------------------------------
actionKey :: DevSection
actionKey =
  DevSection
    { name = "Action Key"
    , description = Nothing
    , operations = [actionKey_cleanActionKeys]
    }

-- ---------------------------------------------------------------------------------------------------------------------
actionKey_cleanActionKeys :: DevOperation
actionKey_cleanActionKeys =
  DevOperation
    { name = "Clean Expired Action Keys"
    , description = Nothing
    , parameters = []
    }

actionKey_cleanActionKeysFn :: DevExecutionDTO -> AppContextM String
actionKey_cleanActionKeysFn reqDto = do
  cleanActionKeys
  return "Done"

-- ---------------------------------------------------------------------------------------------------------------------
-- BRANCH
-- ---------------------------------------------------------------------------------------------------------------------
branch :: DevSection
branch =
  DevSection
    { name = "Branch"
    , description = Nothing
    , operations = [branch_squashAllEvents, branch_squashEventsForBranch]
    }

-- ---------------------------------------------------------------------------------------------------------------------
branch_squashAllEvents :: DevOperation
branch_squashAllEvents =
  DevOperation
    { name = "Squash All Events"
    , description = Nothing
    , parameters = []
    }

branch_squashAllEventsFn :: DevExecutionDTO -> AppContextM String
branch_squashAllEventsFn reqDto = do
  squashEvents
  return "Done"

-- ---------------------------------------------------------------------------------------------------------------------
branch_squashEventsForBranch :: DevOperation
branch_squashEventsForBranch =
  DevOperation
    { name = "Squash Events for Branch"
    , description = Nothing
    , parameters =
        [ DevOperationParameter
            { name = "branchUuid"
            , aType = StringDevOperationParameterType
            }
        ]
    }

branch_squashEventsForBranchFn :: DevExecutionDTO -> AppContextM String
branch_squashEventsForBranchFn reqDto = do
  squashEventsForBranch (head reqDto.parameters)
  return "Done"

-- ---------------------------------------------------------------------------------------------------------------------
-- CACHE
-- ---------------------------------------------------------------------------------------------------------------------
cache :: DevSection
cache =
  DevSection {name = "Cache", description = Nothing, operations = [cache_purgeCache]}

-- ---------------------------------------------------------------------------------------------------------------------
cache_purgeCache :: DevOperation
cache_purgeCache =
  DevOperation
    { name = "Purge All Caches"
    , description = Nothing
    , parameters = []
    }

cache_purgeCacheFn :: DevExecutionDTO -> AppContextM String
cache_purgeCacheFn reqDto = do
  purgeCache
  return "Done"

-- ---------------------------------------------------------------------------------------------------------------------
-- CONFIG
-- ---------------------------------------------------------------------------------------------------------------------
config :: DevSection
config =
  DevSection
    { name = "Config"
    , description = Nothing
    , operations =
        [config_recompileCssInAllApplications, config_switchClientCustomizationOn, config_switchClientCustomizationOff]
    }

-- ---------------------------------------------------------------------------------------------------------------------
config_recompileCssInAllApplications :: DevOperation
config_recompileCssInAllApplications =
  DevOperation
    { name = "Recompile CSS in All Applications"
    , description = Nothing
    , parameters = []
    }

config_recompileCssInAllApplicationsFn :: DevExecutionDTO -> AppContextM String
config_recompileCssInAllApplicationsFn reqDto = do
  recompileCssInAllApplications
  return "Done"

-- ---------------------------------------------------------------------------------------------------------------------
config_switchClientCustomizationOn :: DevOperation
config_switchClientCustomizationOn =
  DevOperation
    { name = "Enable Client Customization in Settings"
    , description = Nothing
    , parameters = []
    }

config_switchClientCustomizationOnFn :: DevExecutionDTO -> AppContextM String
config_switchClientCustomizationOnFn reqDto = do
  modifyClientCustomization True
  return "Done"

-- ---------------------------------------------------------------------------------------------------------------------
config_switchClientCustomizationOff :: DevOperation
config_switchClientCustomizationOff =
  DevOperation
    { name = "Disable Client Customization in Settings"
    , description = Nothing
    , parameters = []
    }

config_switchClientCustomizationOffFn :: DevExecutionDTO -> AppContextM String
config_switchClientCustomizationOffFn reqDto = do
  modifyClientCustomization False
  return "Done"

-- ---------------------------------------------------------------------------------------------------------------------
-- FEEDBACK
-- ---------------------------------------------------------------------------------------------------------------------
feedback :: DevSection
feedback =
  DevSection
    { name = "Feedback"
    , description = Nothing
    , operations = [feedback_synchronizeFeedbacks]
    }

-- ---------------------------------------------------------------------------------------------------------------------
feedback_synchronizeFeedbacks :: DevOperation
feedback_synchronizeFeedbacks =
  DevOperation
    { name = "Synchronize Feedbacks"
    , description = Nothing
    , parameters = []
    }

feedback_synchronizeFeedbacksFn :: DevExecutionDTO -> AppContextM String
feedback_synchronizeFeedbacksFn reqDto = do
  synchronizeFeedbacksInAllApplications
  return "Done"

-- ---------------------------------------------------------------------------------------------------------------------
-- PERSISTENT COMMAND
-- ---------------------------------------------------------------------------------------------------------------------
persistentCommand :: DevSection
persistentCommand =
  DevSection
    { name = "Persistent Command"
    , description = Nothing
    , operations = [persistentCommand_runAll, persistentCommand_run]
    }

-- ---------------------------------------------------------------------------------------------------------------------
persistentCommand_runAll :: DevOperation
persistentCommand_runAll =
  DevOperation
    { name = "Run All Persistent Commands"
    , description = Nothing
    , parameters = []
    }

persistentCommand_runAllFn :: DevExecutionDTO -> AppContextM String
persistentCommand_runAllFn reqDto = do
  context <- ask
  apps <- findApps
  let appUuids = fmap (.uuid) apps
  liftIO $ traverse_ (runAppContextWithBaseContext' runPersistentCommands (baseContextFromAppContext context)) appUuids
  return "Done"

-- ---------------------------------------------------------------------------------------------------------------------
persistentCommand_run :: DevOperation
persistentCommand_run =
  DevOperation
    { name = "Run Persistent Command"
    , description = Nothing
    , parameters =
        [ DevOperationParameter
            { name = "uuid"
            , aType = StringDevOperationParameterType
            }
        ]
    }

persistentCommand_runFn :: DevExecutionDTO -> AppContextM String
persistentCommand_runFn reqDto = do
  command <- findPersistentCommandSimpleByUuid (head reqDto.parameters)
  runPersistentCommand True command
  return "Done"

-- ---------------------------------------------------------------------------------------------------------------------
-- REGISTRY
-- ---------------------------------------------------------------------------------------------------------------------
registry :: DevSection
registry =
  DevSection
    { name = "Registry"
    , description = Nothing
    , operations = [registry_syncWithRegistry]
    }

-- ---------------------------------------------------------------------------------------------------------------------
registry_syncWithRegistry :: DevOperation
registry_syncWithRegistry =
  DevOperation
    { name = "Sync with registry"
    , description = Nothing
    , parameters = []
    }

registry_syncWithRegistryFn :: DevExecutionDTO -> AppContextM String
registry_syncWithRegistryFn reqDto = do
  synchronizeData
  return "Done"

-- ---------------------------------------------------------------------------------------------------------------------
-- QUESTIONNAIRE
-- ---------------------------------------------------------------------------------------------------------------------
questionnaire :: DevSection
questionnaire =
  DevSection
    { name = "Questionnaire"
    , description = Nothing
    , operations =
        [ questionnaire_recomputeQuestionnaireIndications
        , questionnaire_squashAllEvents
        , questionnaire_squashEventsForQuestionnaire
        ]
    }

-- ---------------------------------------------------------------------------------------------------------------------
questionnaire_recomputeQuestionnaireIndications :: DevOperation
questionnaire_recomputeQuestionnaireIndications =
  DevOperation
    { name = "Recompute Questionnaire Indications"
    , description = Nothing
    , parameters = []
    }

questionnaire_recomputeQuestionnaireIndicationsFn :: DevExecutionDTO -> AppContextM String
questionnaire_recomputeQuestionnaireIndicationsFn reqDto = do
  recomputeQuestionnaireIndicationsInAllApplications
  return "Done"

-- ---------------------------------------------------------------------------------------------------------------------
questionnaire_squashAllEvents :: DevOperation
questionnaire_squashAllEvents =
  DevOperation
    { name = "Squash All Events"
    , description = Nothing
    , parameters = []
    }

questionnaire_squashAllEventsFn :: DevExecutionDTO -> AppContextM String
questionnaire_squashAllEventsFn reqDto = do
  squashQuestionnaireEvents
  return "Done"

-- ---------------------------------------------------------------------------------------------------------------------
questionnaire_squashEventsForQuestionnaire :: DevOperation
questionnaire_squashEventsForQuestionnaire =
  DevOperation
    { name = "Squash Events for Questionnaire"
    , description = Nothing
    , parameters =
        [ DevOperationParameter
            { name = "questionnaireUuid"
            , aType = StringDevOperationParameterType
            }
        ]
    }

questionnaire_squashEventsForQuestionnaireFn :: DevExecutionDTO -> AppContextM String
questionnaire_squashEventsForQuestionnaireFn reqDto = do
  squashQuestionnaireEventsForQuestionnaire (head reqDto.parameters)
  return "Done"

-- ---------------------------------------------------------------------------------------------------------------------
-- USER
-- ---------------------------------------------------------------------------------------------------------------------
user :: DevSection
user =
  DevSection
    { name = "User"
    , description = Nothing
    , operations = [user_cleanTokens]
    }

-- ---------------------------------------------------------------------------------------------------------------------
user_cleanTokens :: DevOperation
user_cleanTokens =
  DevOperation
    { name = "Delete expired token"
    , description = Nothing
    , parameters = []
    }

user_cleanTokensFn :: DevExecutionDTO -> AppContextM String
user_cleanTokensFn reqDto = do
  cleanTokens
  return "Done"
