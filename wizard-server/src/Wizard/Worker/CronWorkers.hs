module Wizard.Worker.CronWorkers where

import Shared.Common.Database.VacuumCleaner
import Shared.Common.Model.Config.ServerConfig
import Shared.PersistentCommand.Service.PersistentCommand.PersistentCommandService
import Shared.Worker.Model.Worker.CronWorker
import Wizard.Cache.CacheUtil
import Wizard.Model.Cache.ServerCache
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.BaseContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Service.ActionKey.ActionKeyService
import Wizard.Service.Branch.Event.BranchEventService hiding (squash)
import Wizard.Service.Document.DocumentCleanService
import Wizard.Service.Feedback.FeedbackService
import Wizard.Service.PersistentCommand.PersistentCommandService
import Wizard.Service.Questionnaire.Comment.QuestionnaireCommentService
import Wizard.Service.Questionnaire.Event.QuestionnaireEventService hiding (squash)
import Wizard.Service.Questionnaire.QuestionnaireService
import Wizard.Service.Registry.RegistryService
import Wizard.Service.UserToken.ApiKey.ApiKeyService
import WizardLib.Public.Service.TemporaryFile.TemporaryFileService
import WizardLib.Public.Service.UserToken.UserTokenService

workers :: [CronWorker BaseContext AppContextM]
workers =
  [ actionKeyWorker
  , squashBranchEventsWorker
  , cacheWorker
  , documentWorker
  , feedbackWorker
  , persistentCommandRetryWorker
  , persistentCommandRetryLambdaWorker
  , cleanQuestionnaireWorker
  , squashQuestionnaireEventsWorker
  , assigneeNotificationWorker
  , registrySyncWorker
  , temporaryFileWorker
  , cleanUserTokenWorker
  , expireUserTokenWorker
  , vacuumCleanerWorker
  ]

-- ------------------------------------------------------------------
actionKeyWorker :: CronWorker BaseContext AppContextM
actionKeyWorker =
  CronWorker
    { name = "ActionKeyWorker"
    , condition = (.serverConfig.actionKey.clean.enabled)
    , cronDefault = "20 0 * * *"
    , cron = (.serverConfig.actionKey.clean.cron)
    , function = cleanActionKeys
    , wrapInTransaction = True
    }

squashBranchEventsWorker :: CronWorker BaseContext AppContextM
squashBranchEventsWorker =
  CronWorker
    { name = "SquashBranchEventsWorker"
    , condition = (.serverConfig.branch.squash.enabled)
    , cronDefault = "*/5 * * * *"
    , cron = (.serverConfig.branch.squash.cron)
    , function = squashEvents
    , wrapInTransaction = True
    }

cacheWorker :: CronWorker BaseContext AppContextM
cacheWorker =
  CronWorker
    { name = "CacheWorker"
    , condition = (.serverConfig.cache.purgeExpired.enabled)
    , cronDefault = "45 * * * *"
    , cron = (.serverConfig.cache.purgeExpired.cron)
    , function = purgeExpiredCache
    , wrapInTransaction = True
    }

documentWorker :: CronWorker BaseContext AppContextM
documentWorker =
  CronWorker
    { name = "DocumentWorker"
    , condition = (.serverConfig.questionnaire.clean.enabled)
    , cronDefault = "0 */4 * * *"
    , cron = (.serverConfig.questionnaire.clean.cron)
    , function = cleanDocuments
    , wrapInTransaction = True
    }

feedbackWorker :: CronWorker BaseContext AppContextM
feedbackWorker =
  CronWorker
    { name = "FeedbackWorker"
    , condition = (.serverConfig.feedback.sync.enabled)
    , cronDefault = "0 2 * * *"
    , cron = (.serverConfig.feedback.sync.cron)
    , function = synchronizeFeedbacksInAllApplications
    , wrapInTransaction = True
    }

persistentCommandRetryWorker :: CronWorker BaseContext AppContextM
persistentCommandRetryWorker =
  CronWorker
    { name = "PersistentCommandRetryWorker"
    , condition = (.serverConfig.persistentCommand.retryJob.enabled)
    , cronDefault = "* * * * *"
    , cron = (.serverConfig.persistentCommand.retryJob.cron)
    , function = runPersistentCommands'
    , wrapInTransaction = False
    }

persistentCommandRetryLambdaWorker :: CronWorker BaseContext AppContextM
persistentCommandRetryLambdaWorker =
  CronWorker
    { name = "persistentCommandRetryLambdaWorker"
    , condition = (.serverConfig.persistentCommand.retryLambdaJob.enabled)
    , cronDefault = "* * * * *"
    , cron = (.serverConfig.persistentCommand.retryLambdaJob.cron)
    , function = retryPersistentCommandsForLambda
    , wrapInTransaction = False
    }

cleanQuestionnaireWorker :: CronWorker BaseContext AppContextM
cleanQuestionnaireWorker =
  CronWorker
    { name = "CleanQuestionnaireWorker"
    , condition = (.serverConfig.questionnaire.clean.enabled)
    , cronDefault = "15 */4 * * *"
    , cron = (.serverConfig.questionnaire.clean.cron)
    , function = cleanQuestionnaires
    , wrapInTransaction = True
    }

squashQuestionnaireEventsWorker :: CronWorker BaseContext AppContextM
squashQuestionnaireEventsWorker =
  CronWorker
    { name = "SquashQuestionnaireEventsWorker"
    , condition = (.serverConfig.questionnaire.squash.enabled)
    , cronDefault = "*/4 * * * *"
    , cron = (.serverConfig.questionnaire.squash.cron)
    , function = squashQuestionnaireEvents
    , wrapInTransaction = True
    }

assigneeNotificationWorker :: CronWorker BaseContext AppContextM
assigneeNotificationWorker =
  CronWorker
    { name = "AssigneeNotificationWorker"
    , condition = (.serverConfig.questionnaire.assigneeNotification.enabled)
    , cronDefault = "*/5 * * * *"
    , cron = (.serverConfig.questionnaire.assigneeNotification.cron)
    , function = sendNotificationToNewAssignees
    , wrapInTransaction = True
    }

registrySyncWorker :: CronWorker BaseContext AppContextM
registrySyncWorker =
  CronWorker
    { name = "RegistryWorker"
    , condition = (.serverConfig.registry.sync.enabled)
    , cronDefault = "*/15 * * * *"
    , cron = (.serverConfig.registry.sync.cron)
    , function = synchronizeData
    , wrapInTransaction = True
    }

temporaryFileWorker :: CronWorker BaseContext AppContextM
temporaryFileWorker =
  CronWorker
    { name = "TemporaryFileWorker"
    , condition = (.serverConfig.temporaryFile.clean.enabled)
    , cronDefault = "25 0 * * *"
    , cron = (.serverConfig.temporaryFile.clean.cron)
    , function = cleanTemporaryFiles
    , wrapInTransaction = True
    }

cleanUserTokenWorker :: CronWorker BaseContext AppContextM
cleanUserTokenWorker =
  CronWorker
    { name = "CleanUserTokenWorker"
    , condition = (.serverConfig.userToken.clean.enabled)
    , cronDefault = "0 3 * * *"
    , cron = (.serverConfig.userToken.clean.cron)
    , function = cleanTokens
    , wrapInTransaction = True
    }

expireUserTokenWorker :: CronWorker BaseContext AppContextM
expireUserTokenWorker =
  CronWorker
    { name = "ExpireUserTokenWorker"
    , condition = (.serverConfig.userToken.expire.enabled)
    , cronDefault = "0 4 * * *"
    , cron = (.serverConfig.userToken.expire.cron)
    , function = expireApiKeys
    , wrapInTransaction = True
    }

vacuumCleanerWorker :: CronWorker BaseContext AppContextM
vacuumCleanerWorker =
  CronWorker
    { name = "VacuumCleanerWorker"
    , condition = (.serverConfig.database.vacuumCleaner.enabled)
    , cronDefault = "45 1 * * *"
    , cron = (.serverConfig.database.vacuumCleaner.cron)
    , function = runVacuumCleaner
    , wrapInTransaction = False
    }
