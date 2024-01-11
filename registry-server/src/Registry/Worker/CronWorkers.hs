module Registry.Worker.CronWorkers where

import Registry.Model.Config.ServerConfig
import Registry.Model.Context.AppContext
import Registry.Model.Context.BaseContext
import Registry.Model.Context.ContextLenses ()
import Registry.Service.PersistentCommand.PersistentCommandService
import Shared.Common.Model.Config.ServerConfig
import Shared.PersistentCommand.Service.PersistentCommand.PersistentCommandService
import Shared.Worker.Model.Worker.CronWorker

workers :: [CronWorker BaseContext AppContextM]
workers =
  [ persistentCommandRetryWorker
  , persistentCommandRetryLambdaWorker
  ]

-- ------------------------------------------------------------------
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
