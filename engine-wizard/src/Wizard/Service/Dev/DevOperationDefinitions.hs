module Wizard.Service.Dev.DevOperationDefinitions where

import Control.Lens ((^.))
import Control.Monad.Reader (ask, liftIO)
import Data.Foldable (traverse_)

import LensesConfig hiding (action, cache, feedback)
import Wizard.Api.Resource.Dev.DevExecutionDTO
import Wizard.Database.DAO.App.AppDAO
import Wizard.Database.DAO.PersistentCommand.PersistentCommandDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.Dev.Dev
import Wizard.Service.Branch.Event.BranchEventService
import Wizard.Service.Cache.CacheService
import qualified Wizard.Service.Cache.KnowledgeModelCache as KnowledgeModelCache
import Wizard.Service.Config.AppConfigCommandExecutor
import Wizard.Service.Config.AppConfigService
import Wizard.Service.Feedback.FeedbackService
import Wizard.Service.PersistentCommand.PersistentCommandService
import Wizard.Service.Questionnaire.Event.QuestionnaireEventService
import Wizard.Service.Questionnaire.QuestionnaireService
import Wizard.Service.Registry.RegistryService
import Wizard.Util.Context

-- ---------------------------------------------------------------------------------------------------------------------
-- BRANCH
-- ---------------------------------------------------------------------------------------------------------------------
branch :: DevSection
branch =
  DevSection
    { _devSectionName = "Branch"
    , _devSectionDescription = Nothing
    , _devSectionOperations = [branch_squashAllEvents, branch_squashEventsForBranch]
    }

-- ---------------------------------------------------------------------------------------------------------------------
branch_squashAllEvents :: DevOperation
branch_squashAllEvents =
  DevOperation
    {_devOperationName = "Squash All Events", _devOperationDescription = Nothing, _devOperationParameters = []}

branch_squashAllEventsFn :: DevExecutionDTO -> AppContextM String
branch_squashAllEventsFn reqDto = do
  squashEvents
  return "Done"

-- ---------------------------------------------------------------------------------------------------------------------
branch_squashEventsForBranch :: DevOperation
branch_squashEventsForBranch =
  DevOperation
    { _devOperationName = "Squash Events for Branch"
    , _devOperationDescription = Nothing
    , _devOperationParameters =
        [ DevOperationParameter
            {_devOperationParameterName = "branchUuid", _devOperationParameterAType = StringDevOperationParameterType}
        ]
    }

branch_squashEventsForBranchFn :: DevExecutionDTO -> AppContextM String
branch_squashEventsForBranchFn reqDto = do
  squashEventsForBranch (head (reqDto ^. parameters))
  return "Done"

-- ---------------------------------------------------------------------------------------------------------------------
-- CACHE
-- ---------------------------------------------------------------------------------------------------------------------
cache :: DevSection
cache =
  DevSection
    { _devSectionName = "Cache"
    , _devSectionDescription = Nothing
    , _devSectionOperations = [cache_purgeCache, cache_KnowledgeModelCache_deleteFromCache']
    }

-- ---------------------------------------------------------------------------------------------------------------------
cache_purgeCache :: DevOperation
cache_purgeCache =
  DevOperation
    {_devOperationName = "Purge All Caches", _devOperationDescription = Nothing, _devOperationParameters = []}

cache_purgeCacheFn :: DevExecutionDTO -> AppContextM String
cache_purgeCacheFn reqDto = do
  purgeCache
  return "Done"

-- ---------------------------------------------------------------------------------------------------------------------
cache_KnowledgeModelCache_deleteFromCache' :: DevOperation
cache_KnowledgeModelCache_deleteFromCache' =
  DevOperation
    { _devOperationName = "Purge Knowledge Model Cache"
    , _devOperationDescription = Nothing
    , _devOperationParameters =
        [ DevOperationParameter
            {_devOperationParameterName = "pkgId", _devOperationParameterAType = StringDevOperationParameterType}
        ]
    }

cache_KnowledgeModelCache_deleteFromCacheFn' :: DevExecutionDTO -> AppContextM String
cache_KnowledgeModelCache_deleteFromCacheFn' reqDto = do
  KnowledgeModelCache.deleteFromCache' (head (reqDto ^. parameters))
  return "Done"

-- ---------------------------------------------------------------------------------------------------------------------
-- CONFIG
-- ---------------------------------------------------------------------------------------------------------------------
config :: DevSection
config =
  DevSection
    { _devSectionName = "Config"
    , _devSectionDescription = Nothing
    , _devSectionOperations =
        [config_recompileCssInAllApplications, config_switchClientCustomizationOn, config_switchClientCustomizationOff]
    }

-- ---------------------------------------------------------------------------------------------------------------------
config_recompileCssInAllApplications :: DevOperation
config_recompileCssInAllApplications =
  DevOperation
    { _devOperationName = "Recompile CSS in All Applications"
    , _devOperationDescription = Nothing
    , _devOperationParameters = []
    }

config_recompileCssInAllApplicationsFn :: DevExecutionDTO -> AppContextM String
config_recompileCssInAllApplicationsFn reqDto = do
  recompileCssInAllApplications
  return "Done"

-- ---------------------------------------------------------------------------------------------------------------------
config_switchClientCustomizationOn :: DevOperation
config_switchClientCustomizationOn =
  DevOperation
    { _devOperationName = "Enable Client Customization in Settings"
    , _devOperationDescription = Nothing
    , _devOperationParameters = []
    }

config_switchClientCustomizationOnFn :: DevExecutionDTO -> AppContextM String
config_switchClientCustomizationOnFn reqDto = do
  modifyClientCustomization True
  return "Done"

-- ---------------------------------------------------------------------------------------------------------------------
config_switchClientCustomizationOff :: DevOperation
config_switchClientCustomizationOff =
  DevOperation
    { _devOperationName = "Disable Client Customization in Settings"
    , _devOperationDescription = Nothing
    , _devOperationParameters = []
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
    { _devSectionName = "Feedback"
    , _devSectionDescription = Nothing
    , _devSectionOperations = [feedback_synchronizeFeedbacks]
    }

-- ---------------------------------------------------------------------------------------------------------------------
feedback_synchronizeFeedbacks :: DevOperation
feedback_synchronizeFeedbacks =
  DevOperation
    {_devOperationName = "Synchronize Feedbacks", _devOperationDescription = Nothing, _devOperationParameters = []}

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
    { _devSectionName = "Persistent Command"
    , _devSectionDescription = Nothing
    , _devSectionOperations = [persistentCommand_runAll, persistentCommand_run]
    }

-- ---------------------------------------------------------------------------------------------------------------------
persistentCommand_runAll :: DevOperation
persistentCommand_runAll =
  DevOperation
    { _devOperationName = "Run All Persistent Commands"
    , _devOperationDescription = Nothing
    , _devOperationParameters = []
    }

persistentCommand_runAllFn :: DevExecutionDTO -> AppContextM String
persistentCommand_runAllFn reqDto = do
  context <- ask
  apps <- findApps
  let appUuids = fmap (^. uuid) apps
  liftIO $ traverse_ (runAppContextWithBaseContext' runPersistentCommands (baseContextFromAppContext context)) appUuids
  return "Done"

-- ---------------------------------------------------------------------------------------------------------------------
persistentCommand_run :: DevOperation
persistentCommand_run =
  DevOperation
    { _devOperationName = "Run Persistent Command"
    , _devOperationDescription = Nothing
    , _devOperationParameters =
        [ DevOperationParameter
            {_devOperationParameterName = "uuid", _devOperationParameterAType = StringDevOperationParameterType}
        ]
    }

persistentCommand_runFn :: DevExecutionDTO -> AppContextM String
persistentCommand_runFn reqDto = do
  command <- findPersistentCommandSimpleByUuid (head (reqDto ^. parameters))
  runPersistentCommand command
  return "Done"

-- ---------------------------------------------------------------------------------------------------------------------
-- REGISTRY
-- ---------------------------------------------------------------------------------------------------------------------
registry :: DevSection
registry =
  DevSection
    { _devSectionName = "Registry"
    , _devSectionDescription = Nothing
    , _devSectionOperations = [registry_syncWithRegistry]
    }

-- ---------------------------------------------------------------------------------------------------------------------
registry_syncWithRegistry :: DevOperation
registry_syncWithRegistry =
  DevOperation
    {_devOperationName = "Sync with registry", _devOperationDescription = Nothing, _devOperationParameters = []}

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
    { _devSectionName = "Questionnaire"
    , _devSectionDescription = Nothing
    , _devSectionOperations =
        [ questionnaire_recomputeQuestionnaireIndications
        , questionnaire_squashAllEvents
        , questionnaire_squashEventsForQuestionnaire
        ]
    }

-- ---------------------------------------------------------------------------------------------------------------------
questionnaire_recomputeQuestionnaireIndications :: DevOperation
questionnaire_recomputeQuestionnaireIndications =
  DevOperation
    { _devOperationName = "Recompute Quetionnaire Indications"
    , _devOperationDescription = Nothing
    , _devOperationParameters = []
    }

questionnaire_recomputeQuestionnaireIndicationsFn :: DevExecutionDTO -> AppContextM String
questionnaire_recomputeQuestionnaireIndicationsFn reqDto = do
  recomputeQuestionnaireIndications
  return "Done"

-- ---------------------------------------------------------------------------------------------------------------------
questionnaire_squashAllEvents :: DevOperation
questionnaire_squashAllEvents =
  DevOperation
    {_devOperationName = "Squash All Events", _devOperationDescription = Nothing, _devOperationParameters = []}

questionnaire_squashAllEventsFn :: DevExecutionDTO -> AppContextM String
questionnaire_squashAllEventsFn reqDto = do
  squashQuestionnaireEvents
  return "Done"

-- ---------------------------------------------------------------------------------------------------------------------
questionnaire_squashEventsForQuestionnaire :: DevOperation
questionnaire_squashEventsForQuestionnaire =
  DevOperation
    { _devOperationName = "Squash Events for Questionnaire"
    , _devOperationDescription = Nothing
    , _devOperationParameters =
        [ DevOperationParameter
            { _devOperationParameterName = "questionnaireUuid"
            , _devOperationParameterAType = StringDevOperationParameterType
            }
        ]
    }

questionnaire_squashEventsForQuestionnaireFn :: DevExecutionDTO -> AppContextM String
questionnaire_squashEventsForQuestionnaireFn reqDto = do
  squashQuestionnaireEventsForQuestionnaire (head (reqDto ^. parameters))
  return "Done"
