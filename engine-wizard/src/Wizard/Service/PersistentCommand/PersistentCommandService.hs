module Wizard.Service.PersistentCommand.PersistentCommandService where

import qualified Control.Exception.Base as E
import Control.Lens ((.~), (?~), (^.))
import Control.Monad (forever, when)
import Control.Monad.Reader (ask, asks, liftIO)
import Data.Aeson (Value(..), toJSON)
import Data.Foldable (traverse_)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Time
import qualified Data.UUID as U
import System.Log.Raven (initRaven, register, stderrFallback)
import System.Log.Raven.Transport.HttpConduit (sendRecord)
import System.Log.Raven.Types (SentryLevel(Error), SentryRecord(..))

import LensesConfig
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.PersistentCommand.PersistentCommandDAO
import Wizard.Database.DAO.User.UserDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.PersistentCommand.PersistentCommand
import Wizard.Model.PersistentCommand.PersistentCommandSimple
import qualified Wizard.Service.Config.AppConfigCommandExecutor as AppConfigCommandExecutor
import Wizard.Service.User.UserMapper
import Wizard.Util.Context
import Wizard.Util.Logger

runPersistentCommands :: AppContextM ()
runPersistentCommands = do
  commands <- findPersistentCommandsByStates
  traverse_ runPersistentCommand commands

runPersistentCommand :: PersistentCommandSimple -> AppContextM ()
runPersistentCommand command = do
  user <- findUserByIdSystem . U.toString $ command ^. createdBy
  context <- ask
  let updatedContext = (appUuid .~ (command ^. appUuid)) . (currentUser ?~ toDTO user) $ context
  executePersistentCommandByUuid (U.toString $ command ^. uuid) updatedContext

executePersistentCommandByUuid :: String -> AppContext -> AppContextM ()
executePersistentCommandByUuid uuid context =
  runInTransaction $ do
    logInfoU _CMP_SERVICE (f' "Running command '%s'" [uuid])
    command <- findPersistentCommandByUuid uuid
    eResult <- liftIO . E.try $ runAppContextWithAppContext (execute command) context
    let (resultState, mErrorMessage) =
          case eResult :: Either E.SomeException (Either String (PersistentCommandState, Maybe String)) of
            Right (Right (resultState, mErrorMessage)) -> (resultState, mErrorMessage)
            Right (Left error) -> (ErrorPersistentCommandState, Just error)
            Left exception -> (ErrorPersistentCommandState, Just . show $ exception)
    now <- liftIO getCurrentTime
    let updatedCommand =
          (updatedAt .~ now) . (attempts .~ ((command ^. attempts) + 1)) . (state .~ resultState) .
          (lastErrorMessage .~ mErrorMessage) $
          command
    when (resultState == ErrorPersistentCommandState) (sendToSentry updatedCommand)
    updatePersistentCommandById updatedCommand
    logInfoU _CMP_SERVICE (f' "Command finished with following state: '%s'" [show resultState])

execute :: PersistentCommand -> AppContextM (PersistentCommandState, Maybe String)
execute command
  | command ^. component == AppConfigCommandExecutor.cComponent = AppConfigCommandExecutor.execute command

runPersistentCommandChannelListener :: AppContextM ()
runPersistentCommandChannelListener = do
  forever $ do
    listenPersistentCommandChannel
    _ <- getChannelNotification
    runPersistentCommands

-- --------------------------------
-- PRIVATE
-- --------------------------------
sendToSentry :: PersistentCommand -> AppContextM ()
sendToSentry command = do
  serverConfig <- asks _appContextServerConfig
  when
    (serverConfig ^. sentry . enabled)
    (do let sentryDsn = serverConfig ^. sentry . dsn
        sentryService <- liftIO $ initRaven sentryDsn id sendRecord stderrFallback
        buildInfoConfig <- asks _appContextBuildInfoConfig
        let buildVersion = buildInfoConfig ^. version
        let message = fromMaybe "" (command ^. lastErrorMessage)
        liftIO $ register sentryService "persistentCommandLogger" Error message (recordUpdate buildVersion command))

recordUpdate :: String -> PersistentCommand -> SentryRecord -> SentryRecord
recordUpdate buildVersion command record =
  let commandUserUuid = U.toString $ command ^. createdBy
      commandUuid = U.toString $ command ^. uuid
      commandAppUuid = U.toString $ command ^. appUuid
   in record
        { srRelease = Just buildVersion
        , srInterfaces =
            HashMap.fromList
              [("sentry.interfaces.User", toJSON $ HashMap.fromList [("id", String . T.pack $ commandUserUuid)])]
        , srTags =
            HashMap.fromList
              [ ("uuid", commandUuid)
              , ("component", command ^. component)
              , ("function", command ^. function)
              , ("appUuid", commandAppUuid)
              ]
        , srExtra =
            HashMap.fromList
              [ ("uuid", String . T.pack $ commandUuid)
              , ("component", String . T.pack $ command ^. component)
              , ("function", String . T.pack $ command ^. function)
              , ("appUuid", String . T.pack $ commandAppUuid)
              , ("attempts", String . T.pack . show $ command ^. attempts)
              , ("maxAttempts", String . T.pack . show $ command ^. maxAttempts)
              , ("body", String . T.pack $ command ^. body)
              , ("lastErrorMessage", String . T.pack . fromMaybe "" $ command ^. lastErrorMessage)
              ]
        }
