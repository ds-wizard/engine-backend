module Wizard.Service.Dev.DevOperationDefinitions where

import Control.Monad.Reader (ask, liftIO)
import Data.Foldable (traverse_)

import Shared.Util.Uuid
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
import Wizard.Service.Document.DocumentCleanService
import Wizard.Service.Feedback.FeedbackService
import Wizard.Service.Owl.OwlService
import Wizard.Service.PersistentCommand.PersistentCommandService
import Wizard.Service.Plan.AppPlanService
import Wizard.Service.Questionnaire.Event.QuestionnaireEventService
import Wizard.Service.Questionnaire.QuestionnaireService
import Wizard.Service.Registry.RegistryService
import Wizard.Service.TemporaryFile.TemporaryFileService
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
-- APP PLAN
-- ---------------------------------------------------------------------------------------------------------------------
appPlan :: DevSection
appPlan =
  DevSection
    { name = "App Plan"
    , description = Nothing
    , operations = [appPlan_recomputePlansForApps]
    }

-- ---------------------------------------------------------------------------------------------------------------------
appPlan_recomputePlansForApps :: DevOperation
appPlan_recomputePlansForApps =
  DevOperation
    { name = "Recompute Plans for Apps"
    , description = Nothing
    , parameters = []
    }

appPlan_recomputePlansForAppsFn :: DevExecutionDTO -> AppContextM String
appPlan_recomputePlansForAppsFn reqDto = do
  recomputePlansForApps
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
  squashEventsForBranch (u' . head $ reqDto.parameters)
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
-- DOCUMENT
-- ---------------------------------------------------------------------------------------------------------------------
document :: DevSection
document =
  DevSection
    { name = "Document"
    , description = Nothing
    , operations = [document_cleanDocuments]
    }

-- ---------------------------------------------------------------------------------------------------------------------
document_cleanDocuments :: DevOperation
document_cleanDocuments =
  DevOperation
    { name = "Clean Expired Documents"
    , description = Nothing
    , parameters = []
    }

document_cleanDocumentsFn :: DevExecutionDTO -> AppContextM String
document_cleanDocumentsFn reqDto = do
  cleanDocuments
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
-- OWL
-- ---------------------------------------------------------------------------------------------------------------------
owl :: DevSection
owl =
  DevSection
    { name = "Owl"
    , description = Nothing
    , operations =
        [ owl_switchOwlOn
        , owl_switchOwlOff
        , owl_setOwlProperties
        ]
    }

-- ---------------------------------------------------------------------------------------------------------------------
owl_switchOwlOn :: DevOperation
owl_switchOwlOn =
  DevOperation
    { name = "Enable OWL feature"
    , description = Nothing
    , parameters = []
    }

owl_switchOwlOnFn :: DevExecutionDTO -> AppContextM String
owl_switchOwlOnFn reqDto = do
  modifyOwlFeature True
  return "Done"

-- ---------------------------------------------------------------------------------------------------------------------
owl_switchOwlOff :: DevOperation
owl_switchOwlOff =
  DevOperation
    { name = "Disable OWL feature"
    , description = Nothing
    , parameters = []
    }

owl_switchOwlOffFn :: DevExecutionDTO -> AppContextM String
owl_switchOwlOffFn reqDto = do
  modifyOwlFeature False
  return "Done"

-- ---------------------------------------------------------------------------------------------------------------------
owl_setOwlProperties :: DevOperation
owl_setOwlProperties =
  DevOperation
    { name = "Set OWL properties"
    , description = Just "If you do not want to fill `previousPackageId`, please fill empty space (`' '`)"
    , parameters =
        [ DevOperationParameter
            { name = "name"
            , aType = StringDevOperationParameterType
            }
        , DevOperationParameter
            { name = "organizationId"
            , aType = StringDevOperationParameterType
            }
        , DevOperationParameter
            { name = "kmId"
            , aType = StringDevOperationParameterType
            }
        , DevOperationParameter
            { name = "version"
            , aType = StringDevOperationParameterType
            }
        , DevOperationParameter
            { name = "previousPackageId"
            , aType = StringDevOperationParameterType
            }
        , DevOperationParameter
            { name = "rootElement"
            , aType = StringDevOperationParameterType
            }
        ]
    }

owl_setOwlPropertiesFn :: DevExecutionDTO -> AppContextM String
owl_setOwlPropertiesFn reqDto = do
  let name = head reqDto.parameters
  let organizationId = reqDto.parameters !! 1
  let kmId = reqDto.parameters !! 2
  let version = reqDto.parameters !! 3
  let previousPackageId =
        case reqDto.parameters !! 4 of
          " " -> Nothing
          p -> Just p
  let rootElement = reqDto.parameters !! 5
  setOwlProperties name organizationId kmId version previousPackageId rootElement
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
  command <- findPersistentCommandSimpleByUuid (u' . head $ reqDto.parameters)
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
    , operations = [registry_syncWithRegistry, registry_pushPackageBundle, registry_pushDocumentTemplateBundle, registry_pushLocaleBundle]
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
registry_pushPackageBundle :: DevOperation
registry_pushPackageBundle =
  DevOperation
    { name = "Push Package Bundle"
    , description = Nothing
    , parameters =
        [ DevOperationParameter
            { name = "id"
            , aType = StringDevOperationParameterType
            }
        ]
    }

registry_pushPackageBundleFn :: DevExecutionDTO -> AppContextM String
registry_pushPackageBundleFn reqDto = do
  pushPackageBundle (head reqDto.parameters)
  return "Done"

-- ---------------------------------------------------------------------------------------------------------------------
registry_pushDocumentTemplateBundle :: DevOperation
registry_pushDocumentTemplateBundle =
  DevOperation
    { name = "Push Document Template Bundle"
    , description = Nothing
    , parameters =
        [ DevOperationParameter
            { name = "id"
            , aType = StringDevOperationParameterType
            }
        ]
    }

registry_pushDocumentTemplateBundleFn :: DevExecutionDTO -> AppContextM String
registry_pushDocumentTemplateBundleFn reqDto = do
  pushDocumentTemplateBundle (head reqDto.parameters)
  return "Done"

-- ---------------------------------------------------------------------------------------------------------------------
registry_pushLocaleBundle :: DevOperation
registry_pushLocaleBundle =
  DevOperation
    { name = "Push Locale Bundle"
    , description = Nothing
    , parameters =
        [ DevOperationParameter
            { name = "id"
            , aType = StringDevOperationParameterType
            }
        ]
    }

registry_pushLocaleBundleFn :: DevExecutionDTO -> AppContextM String
registry_pushLocaleBundleFn reqDto = do
  pushLocaleBundle (head reqDto.parameters)
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
        [ questionnaire_cleanQuestionnaires
        , questionnaire_recomputeQuestionnaireIndications
        , questionnaire_squashAllEvents
        , questionnaire_squashEventsForQuestionnaire
        ]
    }

-- ---------------------------------------------------------------------------------------------------------------------
questionnaire_cleanQuestionnaires :: DevOperation
questionnaire_cleanQuestionnaires =
  DevOperation
    { name = "Clean Questionnaires"
    , description = Nothing
    , parameters = []
    }

questionnaire_cleanQuestionnairesFn :: DevExecutionDTO -> AppContextM String
questionnaire_cleanQuestionnairesFn reqDto = do
  cleanQuestionnaires
  return "Done"

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
  squashQuestionnaireEventsForQuestionnaire (u' . head $ reqDto.parameters)
  return "Done"

-- ---------------------------------------------------------------------------------------------------------------------
-- TEMPORARY FILE
-- ---------------------------------------------------------------------------------------------------------------------
temporaryFile :: DevSection
temporaryFile =
  DevSection
    { name = "Temporary File"
    , description = Nothing
    , operations = [temporaryFile_cleanTemporaryFiles]
    }

-- ---------------------------------------------------------------------------------------------------------------------
temporaryFile_cleanTemporaryFiles :: DevOperation
temporaryFile_cleanTemporaryFiles =
  DevOperation
    { name = "Clean Expired Temporary Files"
    , description = Nothing
    , parameters = []
    }

temporaryFile_cleanTemporaryFilesFn :: DevExecutionDTO -> AppContextM String
temporaryFile_cleanTemporaryFilesFn reqDto = do
  cleanTemporaryFiles
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
    { name = "Clean Expired Tokens"
    , description = Nothing
    , parameters = []
    }

user_cleanTokensFn :: DevExecutionDTO -> AppContextM String
user_cleanTokensFn reqDto = do
  cleanTokens
  return "Done"
