module Wizard.Service.Dev.DevOperationDefinitions where

import Control.Monad.Reader (ask, liftIO)
import Data.Foldable (traverse_)

import Shared.Common.Api.Resource.Dev.DevExecutionDTO
import Shared.Common.Model.Dev.Dev
import Shared.Common.Util.Uuid
import Shared.PersistentCommand.Database.DAO.PersistentCommand.PersistentCommandDAO
import Wizard.Cache.CacheUtil
import Wizard.Database.DAO.Tenant.TenantDAO
import Wizard.Model.Cache.ServerCache hiding (user)
import Wizard.Model.Context.AppContext hiding (cache)
import Wizard.Model.Context.ContextMappers
import Wizard.Model.Tenant.Tenant
import Wizard.Service.ActionKey.ActionKeyService
import Wizard.Service.Branch.Event.BranchEventService
import Wizard.Service.Document.DocumentCleanService
import Wizard.Service.Feedback.FeedbackService
import Wizard.Service.Migration.Metamodel.MigratorService
import Wizard.Service.Owl.OwlService
import Wizard.Service.PersistentCommand.PersistentCommandService
import Wizard.Service.Questionnaire.Comment.QuestionnaireCommentService
import Wizard.Service.Questionnaire.Event.QuestionnaireEventService
import Wizard.Service.Questionnaire.QuestionnaireService
import Wizard.Service.Registry.RegistryService
import Wizard.Service.UserToken.ApiKey.ApiKeyService
import WizardLib.Public.Service.TemporaryFile.TemporaryFileService
import WizardLib.Public.Service.UserToken.UserTokenService

sections :: [DevSection AppContextM]
sections =
  [ actionKey
  , apiKey
  , branch
  , cache
  , document
  , feedback
  , metamodelMigrator
  , owl
  , persistentCommand
  , registry
  , questionnaire
  , temporaryFile
  , user
  ]

-- ---------------------------------------------------------------------------------------------------------------------
-- ACTION KEY
-- ---------------------------------------------------------------------------------------------------------------------
actionKey :: DevSection AppContextM
actionKey =
  DevSection
    { name = "Action Key"
    , description = Nothing
    , operations = [actionKey_cleanActionKeys]
    }

-- ---------------------------------------------------------------------------------------------------------------------
actionKey_cleanActionKeys :: DevOperation AppContextM
actionKey_cleanActionKeys =
  DevOperation
    { name = "Clean Expired Action Keys"
    , description = Nothing
    , parameters = []
    , function = \reqDto -> do
        cleanActionKeys
        return "Done"
    }

-- ---------------------------------------------------------------------------------------------------------------------
-- API KEY
-- ---------------------------------------------------------------------------------------------------------------------
apiKey :: DevSection AppContextM
apiKey =
  DevSection
    { name = "API Key"
    , description = Nothing
    , operations = [apiKey_expireApiKeys]
    }

-- ---------------------------------------------------------------------------------------------------------------------
apiKey_expireApiKeys :: DevOperation AppContextM
apiKey_expireApiKeys =
  DevOperation
    { name = "Send Api Key Expiration Mails"
    , description = Nothing
    , parameters = []
    , function = \reqDto -> do
        expireApiKeys
        return "Done"
    }

-- ---------------------------------------------------------------------------------------------------------------------
-- BRANCH
-- ---------------------------------------------------------------------------------------------------------------------
branch :: DevSection AppContextM
branch =
  DevSection
    { name = "Branch"
    , description = Nothing
    , operations =
        [ branch_squashAllEvents
        , branch_squashEventsForBranch
        ]
    }

-- ---------------------------------------------------------------------------------------------------------------------
branch_squashAllEvents :: DevOperation AppContextM
branch_squashAllEvents =
  DevOperation
    { name = "Squash All Events"
    , description = Nothing
    , parameters = []
    , function = \reqDto -> do
        squashEvents
        return "Done"
    }

-- ---------------------------------------------------------------------------------------------------------------------
branch_squashEventsForBranch :: DevOperation AppContextM
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
    , function = \reqDto -> do
        squashEventsForBranch (u' . head $ reqDto.parameters)
        return "Done"
    }

-- ---------------------------------------------------------------------------------------------------------------------
-- CACHE
-- ---------------------------------------------------------------------------------------------------------------------
cache :: DevSection AppContextM
cache =
  DevSection
    { name = "Cache"
    , description = Nothing
    , operations =
        [ cache_purgeCache
        , cache_getUserCacheSize
        , cache_getUserTokenCacheSize
        ]
    }

-- ---------------------------------------------------------------------------------------------------------------------
cache_purgeCache :: DevOperation AppContextM
cache_purgeCache =
  DevOperation
    { name = "Purge All Caches"
    , description = Nothing
    , parameters = []
    , function = \reqDto -> do
        purgeCache
        return "Done"
    }

-- ---------------------------------------------------------------------------------------------------------------------
cache_getUserCacheSize :: DevOperation AppContextM
cache_getUserCacheSize =
  DevOperation
    { name = "Get User Cache Size"
    , description = Nothing
    , parameters = []
    , function = const computeUserCacheSize
    }

-- ---------------------------------------------------------------------------------------------------------------------
cache_getUserTokenCacheSize :: DevOperation AppContextM
cache_getUserTokenCacheSize =
  DevOperation
    { name = "Get User Token Cache Size"
    , description = Nothing
    , parameters = []
    , function = const computeUserTokenCacheSize
    }

-- ---------------------------------------------------------------------------------------------------------------------
-- DOCUMENT
-- ---------------------------------------------------------------------------------------------------------------------
document :: DevSection AppContextM
document =
  DevSection
    { name = "Document"
    , description = Nothing
    , operations = [document_cleanDocuments]
    }

-- ---------------------------------------------------------------------------------------------------------------------
document_cleanDocuments :: DevOperation AppContextM
document_cleanDocuments =
  DevOperation
    { name = "Clean Expired Documents"
    , description = Nothing
    , parameters = []
    , function = \reqDto -> do
        cleanDocuments
        return "Done"
    }

-- ---------------------------------------------------------------------------------------------------------------------
-- FEEDBACK
-- ---------------------------------------------------------------------------------------------------------------------
feedback :: DevSection AppContextM
feedback =
  DevSection
    { name = "Feedback"
    , description = Nothing
    , operations = [feedback_synchronizeFeedbacks]
    }

-- ---------------------------------------------------------------------------------------------------------------------
feedback_synchronizeFeedbacks :: DevOperation AppContextM
feedback_synchronizeFeedbacks =
  DevOperation
    { name = "Synchronize Feedbacks"
    , description = Nothing
    , parameters = []
    , function = \reqDto -> do
        synchronizeFeedbacksInAllApplications
        return "Done"
    }

-- ---------------------------------------------------------------------------------------------------------------------
-- FEEDBACK
-- ---------------------------------------------------------------------------------------------------------------------
metamodelMigrator :: DevSection AppContextM
metamodelMigrator =
  DevSection
    { name = "Metamodel Migrator"
    , description = Nothing
    , operations = [metamodelMigrator_migrate]
    }

-- ---------------------------------------------------------------------------------------------------------------------
metamodelMigrator_migrate :: DevOperation AppContextM
metamodelMigrator_migrate =
  DevOperation
    { name = "Migrate"
    , description = Nothing
    , parameters =
        [ DevOperationParameter
            { name = "tenantUuid"
            , aType = StringDevOperationParameterType
            }
        ]
    , function = \reqDto -> do
        let tenantUuid = u' . head $ reqDto.parameters
        tenant <- findTenantByUuid tenantUuid
        migrateToLatestMetamodelVersionCommand tenant Nothing
        return "Done"
    }

-- ---------------------------------------------------------------------------------------------------------------------
-- OWL
-- ---------------------------------------------------------------------------------------------------------------------
owl :: DevSection AppContextM
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
owl_switchOwlOn :: DevOperation AppContextM
owl_switchOwlOn =
  DevOperation
    { name = "Enable OWL feature"
    , description = Nothing
    , parameters = []
    , function = \reqDto -> do
        modifyOwlFeature True
        return "Done"
    }

-- ---------------------------------------------------------------------------------------------------------------------
owl_switchOwlOff :: DevOperation AppContextM
owl_switchOwlOff =
  DevOperation
    { name = "Disable OWL feature"
    , description = Nothing
    , parameters = []
    , function = \reqDto -> do
        modifyOwlFeature False
        return "Done"
    }

-- ---------------------------------------------------------------------------------------------------------------------
owl_setOwlProperties :: DevOperation AppContextM
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
    , function = \reqDto -> do
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
    }

-- ---------------------------------------------------------------------------------------------------------------------
-- PERSISTENT COMMAND
-- ---------------------------------------------------------------------------------------------------------------------
persistentCommand :: DevSection AppContextM
persistentCommand =
  DevSection
    { name = "Persistent Command"
    , description = Nothing
    , operations = [persistentCommand_runAll, persistentCommand_run]
    }

-- ---------------------------------------------------------------------------------------------------------------------
persistentCommand_runAll :: DevOperation AppContextM
persistentCommand_runAll =
  DevOperation
    { name = "Run All Persistent Commands"
    , description = Nothing
    , parameters = []
    , function = \reqDto -> do
        context <- ask
        tenants <- findTenants
        let tenantUuids = fmap (.uuid) tenants
        liftIO $ traverse_ (runAppContextWithBaseContext' runPersistentCommands' (baseContextFromAppContext context)) tenantUuids
        return "Done"
    }

-- ---------------------------------------------------------------------------------------------------------------------
persistentCommand_run :: DevOperation AppContextM
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
    , function = \reqDto -> do
        command <- findPersistentCommandSimpleByUuid (u' . head $ reqDto.parameters)
        runPersistentCommand' True command
        return "Done"
    }

-- ---------------------------------------------------------------------------------------------------------------------
-- REGISTRY
-- ---------------------------------------------------------------------------------------------------------------------
registry :: DevSection AppContextM
registry =
  DevSection
    { name = "Registry"
    , description = Nothing
    , operations = [registry_syncWithRegistry, registry_pushPackageBundle, registry_pushDocumentTemplateBundle, registry_pushLocaleBundle]
    }

-- ---------------------------------------------------------------------------------------------------------------------
registry_syncWithRegistry :: DevOperation AppContextM
registry_syncWithRegistry =
  DevOperation
    { name = "Sync with registry"
    , description = Nothing
    , parameters = []
    , function = \reqDto -> do
        synchronizeData
        return "Done"
    }

-- ---------------------------------------------------------------------------------------------------------------------
registry_pushPackageBundle :: DevOperation AppContextM
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
    , function = \reqDto -> do
        pushPackageBundle (head reqDto.parameters)
        return "Done"
    }

-- ---------------------------------------------------------------------------------------------------------------------
registry_pushDocumentTemplateBundle :: DevOperation AppContextM
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
    , function = \reqDto -> do
        pushDocumentTemplateBundle (head reqDto.parameters)
        return "Done"
    }

-- ---------------------------------------------------------------------------------------------------------------------
registry_pushLocaleBundle :: DevOperation AppContextM
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
    , function = \reqDto -> do
        pushLocaleBundle (head reqDto.parameters)
        return "Done"
    }

-- ---------------------------------------------------------------------------------------------------------------------
-- QUESTIONNAIRE
-- ---------------------------------------------------------------------------------------------------------------------
questionnaire :: DevSection AppContextM
questionnaire =
  DevSection
    { name = "Questionnaire"
    , description = Nothing
    , operations =
        [ questionnaire_cleanQuestionnaires
        , questionnaire_squashAllEvents
        , questionnaire_squashEventsForQuestionnaire
        , questionnaire_sendNotificationToNewAssignees
        ]
    }

-- ---------------------------------------------------------------------------------------------------------------------
questionnaire_cleanQuestionnaires :: DevOperation AppContextM
questionnaire_cleanQuestionnaires =
  DevOperation
    { name = "Clean Questionnaires"
    , description = Nothing
    , parameters = []
    , function = \reqDto -> do
        cleanQuestionnaires
        return "Done"
    }

-- ---------------------------------------------------------------------------------------------------------------------
questionnaire_squashAllEvents :: DevOperation AppContextM
questionnaire_squashAllEvents =
  DevOperation
    { name = "Squash All Events"
    , description = Nothing
    , parameters = []
    , function = \reqDto -> do
        squashQuestionnaireEvents
        return "Done"
    }

-- ---------------------------------------------------------------------------------------------------------------------
questionnaire_squashEventsForQuestionnaire :: DevOperation AppContextM
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
    , function = \reqDto -> do
        squashQuestionnaireEventsForQuestionnaire (u' . head $ reqDto.parameters)
        return "Done"
    }

-- ---------------------------------------------------------------------------------------------------------------------
questionnaire_sendNotificationToNewAssignees :: DevOperation AppContextM
questionnaire_sendNotificationToNewAssignees =
  DevOperation
    { name = "Send Notification to New Assignees"
    , description = Nothing
    , parameters = []
    , function = \reqDto -> do
        sendNotificationToNewAssignees
        return "Done"
    }

-- ---------------------------------------------------------------------------------------------------------------------
-- TEMPORARY FILE
-- ---------------------------------------------------------------------------------------------------------------------
temporaryFile :: DevSection AppContextM
temporaryFile =
  DevSection
    { name = "Temporary File"
    , description = Nothing
    , operations = [temporaryFile_cleanTemporaryFiles]
    }

-- ---------------------------------------------------------------------------------------------------------------------
temporaryFile_cleanTemporaryFiles :: DevOperation AppContextM
temporaryFile_cleanTemporaryFiles =
  DevOperation
    { name = "Clean Expired Temporary Files"
    , description = Nothing
    , parameters = []
    , function = \reqDto -> do
        cleanTemporaryFiles
        return "Done"
    }

-- ---------------------------------------------------------------------------------------------------------------------
-- USER
-- ---------------------------------------------------------------------------------------------------------------------
user :: DevSection AppContextM
user =
  DevSection
    { name = "User"
    , description = Nothing
    , operations = [user_cleanTokens]
    }

-- ---------------------------------------------------------------------------------------------------------------------
user_cleanTokens :: DevOperation AppContextM
user_cleanTokens =
  DevOperation
    { name = "Clean Expired Tokens"
    , description = Nothing
    , parameters = []
    , function = \reqDto -> do
        cleanTokens
        return "Done"
    }
