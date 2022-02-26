module Wizard.Service.PersistentCommand.PersistentCommandService where

import qualified Control.Exception.Base as E
import Control.Lens ((.~), (?~), (^.))
import Control.Monad (forever)
import Control.Monad.Reader (ask, liftIO)
import Data.Foldable (traverse_)
import Data.Time
import qualified Data.UUID as U

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
            Right (Left error) -> (ErrorPersistentCommandState, Just . show $ error)
            Left exception -> (ErrorPersistentCommandState, Just . show $ exception)
    now <- liftIO getCurrentTime
    let updatedCommand =
          (updatedAt .~ now) .
          (attempts .~ ((command ^. attempts) + 1)) . (state .~ resultState) . (lastErrorMessage .~ mErrorMessage) $
          command
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
