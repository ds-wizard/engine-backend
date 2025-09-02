module Shared.PersistentCommand.Service.PersistentCommand.PersistentCommandService where

import qualified Control.Exception.Base as E
import Control.Monad (forever, unless, void, when)
import Control.Monad.Reader (ask, liftIO)
import Data.Aeson (Value (..), toJSON)
import Data.Foldable (traverse_)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as L
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Time
import qualified Data.UUID as U
import System.Log.Raven (initRaven, register, stderrFallback)
import System.Log.Raven.Transport.HttpConduit (sendRecord)
import System.Log.Raven.Types (SentryLevel (Error), SentryRecord (..))

import Shared.Common.Model.Config.BuildInfoConfig
import Shared.Common.Model.Config.ServerConfig
import Shared.Common.Model.Context.AppContext
import Shared.Common.Service.Acl.AclService
import Shared.Common.Util.Error (tryError)
import Shared.Common.Util.Logger
import Shared.PersistentCommand.Database.DAO.PersistentCommand.PersistentCommandDAO
import Shared.PersistentCommand.Model.PersistentCommand.PersistentCommand
import Shared.PersistentCommand.Model.PersistentCommand.PersistentCommandSimple
import Shared.PersistentCommand.Service.PersistentCommand.PersistentCommandMapper

runPersistentCommands
  :: AppContextC s sc m
  => (function -> appContext -> IO (Either String (PersistentCommandState, Maybe String)))
  -> (PersistentCommandSimple U.UUID -> s -> m appContext)
  -> (String -> PersistentCommand U.UUID -> m a)
  -> (PersistentCommand U.UUID -> function)
  -> m ()
runPersistentCommands runAppContextWithAppContext' updateContext createPersistentCommand execute = do
  checkPermission _DEV_PERM
  commands <- findPersistentCommandsForRetryByStates
  unless
    (null commands)
    ( do
        traverse_ (runPersistentCommand runAppContextWithAppContext' updateContext createPersistentCommand execute False) commands
        runPersistentCommands runAppContextWithAppContext' updateContext createPersistentCommand execute
    )

runPersistentCommand
  :: AppContextC s sc m
  => (function -> appContext -> IO (Either String (PersistentCommandState, Maybe String)))
  -> (PersistentCommandSimple U.UUID -> s -> m appContext)
  -> (String -> PersistentCommand U.UUID -> m a)
  -> (PersistentCommand U.UUID -> function)
  -> Bool
  -> PersistentCommandSimple U.UUID
  -> m ()
runPersistentCommand runAppContextWithAppContext' updateContext createPersistentCommand execute force commandSimple = do
  case commandSimple.destination of
    Just destination -> transferPersistentCommandByUuid destination createPersistentCommand commandSimple.uuid
    Nothing -> do
      context <- ask
      updatedContext <- updateContext commandSimple context
      executePersistentCommandByUuid runAppContextWithAppContext' execute force commandSimple.uuid updatedContext

executePersistentCommandByUuid
  :: AppContextC s sc m
  => (function -> appContext -> IO (Either String (PersistentCommandState, Maybe String)))
  -> (PersistentCommand U.UUID -> function)
  -> Bool
  -> U.UUID
  -> appContext
  -> m ()
executePersistentCommandByUuid runAppContextWithAppContext' execute force uuid context = do
  logInfoI _CMP_SERVICE (f' "Running command '%s'" [U.toString uuid])
  command <- findPersistentCommandByUuid uuid
  when
    (command.state == NewPersistentCommandState || (command.state == ErrorPersistentCommandState && command.attempts < command.maxAttempts) || force)
    ( do
        eResult <- liftIO . E.try $ runAppContextWithAppContext' (execute command) context
        let (resultState, mErrorMessage) =
              case eResult :: Either E.SomeException (Either String (PersistentCommandState, Maybe String)) of
                Right (Right (resultState, mErrorMessage)) -> (resultState, mErrorMessage)
                Right (Left error) -> (ErrorPersistentCommandState, Just error)
                Left exception -> (ErrorPersistentCommandState, Just . show $ exception)
        context <- ask
        now <- liftIO getCurrentTime
        let updatedCommand =
              command
                { state = resultState
                , lastTraceUuid = Just context.traceUuid'
                , lastErrorMessage = mErrorMessage
                , attempts = command.attempts + 1
                , updatedAt = now
                }
              :: PersistentCommand U.UUID
        when (resultState == ErrorPersistentCommandState) (sendToSentry updatedCommand)
        updatePersistentCommandByUuid updatedCommand
        logInfoI _CMP_SERVICE (f' "Command finished with following state: '%s'" [show resultState])
    )

transferPersistentCommandByUuid
  :: AppContextC s sc m
  => String
  -> (String -> PersistentCommand U.UUID -> m a)
  -> U.UUID
  -> m ()
transferPersistentCommandByUuid destination createPersistentCommand uuid = do
  logInfoI _CMP_SERVICE (f' "Transferring command '%s'" [U.toString uuid])
  command <- findPersistentCommandByUuid uuid
  when
    (command.attempts < command.maxAttempts)
    ( do
        let sanitizedCommand = sanitizePersistentCommand command
        eResult <- tryError (createPersistentCommand destination sanitizedCommand)
        let (resultState, mErrorMessage) =
              case eResult of
                Right _ -> (DonePersistentCommandState, Nothing)
                Left exception -> (ErrorPersistentCommandState, Just . show $ exception)
        context <- ask
        now <- liftIO getCurrentTime
        let updatedCommand =
              command
                { state = resultState
                , lastTraceUuid = Just context.traceUuid'
                , lastErrorMessage = mErrorMessage
                , attempts = command.attempts + 1
                , updatedAt = now
                }
              :: PersistentCommand U.UUID
        when (resultState == ErrorPersistentCommandState) (sendToSentry updatedCommand)
        updatePersistentCommandByUuid updatedCommand
        logInfoI _CMP_SERVICE (f' "Command transferred with following state: '%s'" [show resultState])
    )

runPersistentCommandChannelListener
  :: AppContextC s sc m
  => (function -> appContext -> IO (Either String (PersistentCommandState, Maybe String)))
  -> (PersistentCommandSimple U.UUID -> s -> m appContext)
  -> (String -> PersistentCommand U.UUID -> m a)
  -> (PersistentCommand U.UUID -> function)
  -> m ()
runPersistentCommandChannelListener runAppContextWithAppContext updateContext createPersistentCommand execute = do
  listenPersistentCommandChannel
  forever $ do
    _ <- getChannelNotification
    runPersistentCommands runAppContextWithAppContext updateContext createPersistentCommand execute

retryPersistentCommandsForLambda :: AppContextC s sc m => m ()
retryPersistentCommandsForLambda = do
  context <- ask
  let components = fmap (\lf -> lf.component) context.serverConfig'.persistentCommand'.lambdaFunctions
  persistentCommands <- findPersistentCommandsForLambdaByStates components
  traverse_ retryPersistentCommandForLambda persistentCommands

retryPersistentCommandForLambda :: AppContextC s sc m => PersistentCommandSimple U.UUID -> m ()
retryPersistentCommandForLambda command = do
  context <- ask
  case L.find (\lf -> lf.component == command.component) (context.serverConfig'.persistentCommand'.lambdaFunctions) of
    Just lf -> void $ invokeLambdaFunction command lf
    Nothing -> logWarnI _CMP_DATABASE (f' "No lambda function found for persistent command '%s'" [U.toString command.uuid])

-- --------------------------------
-- PRIVATE
-- --------------------------------
sendToSentry
  :: AppContextC s sc m
  => PersistentCommand U.UUID
  -> m ()
sendToSentry command = do
  context <- ask
  when
    (context.serverConfig'.sentry'.enabled)
    ( do
        let sentryDsn = context.serverConfig'.sentry'.dsn
        sentryService <- liftIO $ initRaven sentryDsn id sendRecord stderrFallback
        let buildVersion = context.buildInfoConfig'.version
        let message = fromMaybe "" command.lastErrorMessage
        liftIO $ register sentryService "persistentCommandLogger" Error message (recordUpdate buildVersion command)
    )

recordUpdate :: String -> PersistentCommand U.UUID -> SentryRecord -> SentryRecord
recordUpdate buildVersion command record =
  let commandUserUuid = maybe "anonymous" U.toString command.createdBy
      commandUuid = U.toString $ command.uuid
      commandTenantUuid = U.toString $ command.tenantUuid
   in record
        { srRelease = Just buildVersion
        , srInterfaces =
            HashMap.fromList
              [("sentry.interfaces.User", toJSON $ HashMap.fromList [("id", String . T.pack $ commandUserUuid)])]
        , srTags =
            HashMap.fromList
              [ ("uuid", commandUuid)
              , ("component", command.component)
              , ("function", command.function)
              , ("tenantUuid", commandTenantUuid)
              ]
        , srExtra =
            HashMap.fromList
              [ ("uuid", String . T.pack $ commandUuid)
              , ("component", String . T.pack $ command.component)
              , ("function", String . T.pack $ command.function)
              , ("tenantUuid", String . T.pack $ commandTenantUuid)
              , ("attempts", String . T.pack . show $ command.attempts)
              , ("maxAttempts", String . T.pack . show $ command.maxAttempts)
              , ("body", String . T.pack $ command.body)
              , ("lastErrorMessage", String . T.pack . fromMaybe "" $ command.lastErrorMessage)
              ]
        }
