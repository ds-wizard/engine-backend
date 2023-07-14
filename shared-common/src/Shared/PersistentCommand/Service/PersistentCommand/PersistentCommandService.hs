module Shared.PersistentCommand.Service.PersistentCommand.PersistentCommandService where

import qualified Control.Exception.Base as E
import Control.Monad (forever, unless, when)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Reader (MonadReader, ask, liftIO)
import Data.Aeson (Value (..), toJSON)
import Data.Foldable (traverse_)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (fromMaybe)
import Data.Pool
import qualified Data.Text as T
import Data.Time
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import GHC.Records
import System.Log.Raven (initRaven, register, stderrFallback)
import System.Log.Raven.Transport.HttpConduit (sendRecord)
import System.Log.Raven.Types (SentryLevel (Error), SentryRecord (..))

import Shared.Common.Database.DAO.Common
import Shared.Common.Model.Config.BuildInfoConfig
import Shared.Common.Model.Config.ServerConfig
import Shared.Common.Model.Error.Error
import Shared.Common.Service.Acl.AclService
import Shared.Common.Util.Error (tryError)
import Shared.Common.Util.Logger
import Shared.PersistentCommand.Database.DAO.PersistentCommand.PersistentCommandDAO
import Shared.PersistentCommand.Model.PersistentCommand.PersistentCommand
import Shared.PersistentCommand.Model.PersistentCommand.PersistentCommandSimple

runPersistentCommands
  :: ( MonadLogger m
     , MonadError AppError m
     , MonadReader s m
     , MonadIO m
     , HasField "dbPool'" s (Pool Connection)
     , HasField "dbConnection'" s (Maybe Connection)
     , HasField "identity'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , HasField "serverConfig'" s sc
     , HasField "sentry" sc ServerConfigSentry
     , HasField "buildInfoConfig'" s BuildInfoConfig
     , AclContext m
     )
  => (function -> appContext -> IO (Either String (PersistentCommandState, Maybe String)))
  -> (PersistentCommandSimple U.UUID -> s -> m appContext)
  -> (PersistentCommand U.UUID -> m a2)
  -> (PersistentCommand U.UUID -> function)
  -> m ()
runPersistentCommands runAppContextWithAppContext updateContext createPersistentCommand execute = do
  checkPermission _DEV_PERM
  commands <- findPersistentCommandsByStates
  unless
    (null commands)
    ( do
        traverse_ (runPersistentCommand runAppContextWithAppContext updateContext createPersistentCommand execute False) commands
        runPersistentCommands runAppContextWithAppContext updateContext createPersistentCommand execute
    )

runPersistentCommand
  :: ( MonadLogger m
     , MonadError AppError m
     , MonadReader s m
     , MonadIO m
     , HasField "dbPool'" s (Pool Connection)
     , HasField "dbConnection'" s (Maybe Connection)
     , HasField "identity'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , HasField "serverConfig'" s sc
     , HasField "sentry" sc ServerConfigSentry
     , HasField "buildInfoConfig'" s BuildInfoConfig
     )
  => (function -> appContext -> IO (Either String (PersistentCommandState, Maybe String)))
  -> (PersistentCommandSimple U.UUID -> s -> m appContext)
  -> (PersistentCommand U.UUID -> m a2)
  -> (PersistentCommand U.UUID -> function)
  -> Bool
  -> PersistentCommandSimple U.UUID
  -> m ()
runPersistentCommand runAppContextWithAppContext updateContext createPersistentCommand execute force commandSimple = do
  case commandSimple.destination of
    Just destination -> tranferPersistentCommandByUuid createPersistentCommand commandSimple.uuid
    Nothing -> do
      context <- ask
      updatedContext <- updateContext commandSimple context
      executePersistentCommandByUuid runAppContextWithAppContext execute force commandSimple.uuid updatedContext

executePersistentCommandByUuid
  :: ( MonadLogger m
     , MonadError AppError m
     , MonadReader s m
     , MonadIO m
     , HasField "dbPool'" s (Pool Connection)
     , HasField "dbConnection'" s (Maybe Connection)
     , HasField "identity'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , HasField "serverConfig'" s sc
     , HasField "sentry" sc ServerConfigSentry
     , HasField "buildInfoConfig'" s BuildInfoConfig
     )
  => (function -> appContext -> IO (Either String (PersistentCommandState, Maybe String)))
  -> (PersistentCommand U.UUID -> function)
  -> Bool
  -> U.UUID
  -> appContext
  -> m ()
executePersistentCommandByUuid runAppContextWithAppContext execute force uuid context =
  runInTransaction logInfoI logWarnI $ do
    logInfoI _CMP_SERVICE (f' "Running command '%s'" [U.toString uuid])
    command <- findPersistentCommandByUuid uuid
    when
      (command.state == NewPersistentCommandState || (command.state == ErrorPersistentCommandState && command.attempts < command.maxAttempts) || force)
      ( do
          eResult <- liftIO . E.try $ runAppContextWithAppContext (execute command) context
          let (resultState, mErrorMessage) =
                case eResult :: Either E.SomeException (Either String (PersistentCommandState, Maybe String)) of
                  Right (Right (resultState, mErrorMessage)) -> (resultState, mErrorMessage)
                  Right (Left error) -> (ErrorPersistentCommandState, Just error)
                  Left exception -> (ErrorPersistentCommandState, Just . show $ exception)
          now <- liftIO getCurrentTime
          let updatedCommand =
                command
                  { state = resultState
                  , lastErrorMessage = mErrorMessage
                  , attempts = command.attempts + 1
                  , updatedAt = now
                  }
                :: PersistentCommand U.UUID
          when (resultState == ErrorPersistentCommandState) (sendToSentry updatedCommand)
          updatePersistentCommandByUuid updatedCommand
          logInfoI _CMP_SERVICE (f' "Command finished with following state: '%s'" [show resultState])
      )

tranferPersistentCommandByUuid
  :: ( MonadLogger m
     , MonadError AppError m
     , MonadReader s m
     , MonadIO m
     , HasField "dbPool'" s (Pool Connection)
     , HasField "dbConnection'" s (Maybe Connection)
     , HasField "identity'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , HasField "serverConfig'" s sc
     , HasField "sentry" sc ServerConfigSentry
     , HasField "buildInfoConfig'" s BuildInfoConfig
     )
  => (PersistentCommand U.UUID -> m a)
  -> U.UUID
  -> m ()
tranferPersistentCommandByUuid createPersistentCommand uuid =
  runInTransaction logInfoI logWarnI $ do
    logInfoI _CMP_SERVICE (f' "Transfering command '%s'" [U.toString uuid])
    command <- findPersistentCommandByUuid uuid
    when
      (command.attempts < command.maxAttempts)
      ( do
          eResult <- tryError (createPersistentCommand command)
          let (resultState, mErrorMessage) =
                case eResult of
                  Right _ -> (DonePersistentCommandState, Nothing)
                  Left exception -> (ErrorPersistentCommandState, Just . show $ exception)
          now <- liftIO getCurrentTime
          let updatedCommand =
                command
                  { state = resultState
                  , lastErrorMessage = mErrorMessage
                  , attempts = command.attempts + 1
                  , updatedAt = now
                  }
                :: PersistentCommand U.UUID
          when (resultState == ErrorPersistentCommandState) (sendToSentry updatedCommand)
          updatePersistentCommandByUuid updatedCommand
          logInfoI _CMP_SERVICE (f' "Command transfered with following state: '%s'" [show resultState])
      )

runPersistentCommandChannelListener
  :: ( MonadLogger m
     , MonadError AppError m
     , MonadReader s m
     , MonadIO m
     , HasField "dbPool'" s (Pool Connection)
     , HasField "dbConnection'" s (Maybe Connection)
     , HasField "identity'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , HasField "serverConfig'" s sc
     , HasField "sentry" sc ServerConfigSentry
     , HasField "buildInfoConfig'" s BuildInfoConfig
     , AclContext m
     )
  => (function -> appContext -> IO (Either String (PersistentCommandState, Maybe String)))
  -> (PersistentCommandSimple U.UUID -> s -> m appContext)
  -> (PersistentCommand U.UUID -> m a2)
  -> (PersistentCommand U.UUID -> function)
  -> m ()
runPersistentCommandChannelListener runAppContextWithAppContext updateContext createPersistentCommand execute = do
  forever $ do
    listenPersistentCommandChannel
    _ <- getChannelNotification
    runPersistentCommands runAppContextWithAppContext updateContext createPersistentCommand execute

-- --------------------------------
-- PRIVATE
-- --------------------------------
sendToSentry
  :: ( MonadReader context m
     , HasField "serverConfig'" context sc
     , HasField "sentry" sc ServerConfigSentry
     , HasField "buildInfoConfig'" context BuildInfoConfig
     , MonadLogger m
     , MonadIO m
     )
  => PersistentCommand U.UUID
  -> m ()
sendToSentry command = do
  context <- ask
  when
    (context.serverConfig'.sentry.enabled)
    ( do
        let sentryDsn = context.serverConfig'.sentry.dsn
        sentryService <- liftIO $ initRaven sentryDsn id sendRecord stderrFallback
        let buildVersion = context.buildInfoConfig'.version
        let message = fromMaybe "" command.lastErrorMessage
        liftIO $ register sentryService "persistentCommandLogger" Error message (recordUpdate buildVersion command)
    )

recordUpdate :: String -> PersistentCommand U.UUID -> SentryRecord -> SentryRecord
recordUpdate buildVersion command record =
  let commandUserUuid = maybe "anonymous" U.toString command.createdBy
      commandUuid = U.toString $ command.uuid
      commandAppUuid = U.toString $ command.appUuid
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
              , ("appUuid", commandAppUuid)
              ]
        , srExtra =
            HashMap.fromList
              [ ("uuid", String . T.pack $ commandUuid)
              , ("component", String . T.pack $ command.component)
              , ("function", String . T.pack $ command.function)
              , ("appUuid", String . T.pack $ commandAppUuid)
              , ("attempts", String . T.pack . show $ command.attempts)
              , ("maxAttempts", String . T.pack . show $ command.maxAttempts)
              , ("body", String . T.pack $ command.body)
              , ("lastErrorMessage", String . T.pack . fromMaybe "" $ command.lastErrorMessage)
              ]
        }
