module Wizard.Service.PersistentCommand.PersistentCommandService where

import Control.Lens ((.~), (^.))
import Control.Monad.Except (catchError)
import Control.Monad.Reader (liftIO)
import Data.Foldable (traverse_)
import Data.Time
import qualified Data.UUID as U

import LensesConfig
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.PersistentCommand.PersistentCommandDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.PersistentCommand.PersistentCommand
import qualified Wizard.Service.Config.AppConfigService as AppConfigService
import Wizard.Util.Logger

allowedComponents = [AppConfigService.cComponent]

runPersistentCommands :: AppContextM ()
runPersistentCommands = do
  commandUuids <- findPersistentCommandsByStatesAndComponents allowedComponents
  traverse_ (runPersistentCommandByUuid . U.toString) commandUuids

runPersistentCommandByUuid :: String -> AppContextM ()
runPersistentCommandByUuid uuid =
  runInTransaction $ do
    logInfoU _CMP_SERVICE (f' "Running command '%s'" [uuid])
    command <- findPersistentCommandByUuid uuid
    (resultState, mErrorMessage) <-
      catchError
        (execute (command ^. component) (command ^. function) command)
        (\error -> return (ErrorPersistentCommandState, Just . show $ error))
    now <- liftIO getCurrentTime
    let updatedCommand =
          (updatedAt .~ now) .
          (attempts .~ ((command ^. attempts) + 1)) . (state .~ resultState) . (lastErrorMessage .~ mErrorMessage) $
          command
    updatePersistentCommandById updatedCommand
    logInfoU _CMP_SERVICE (f' "Command finish with following state: '%s'" [show resultState])

execute :: String -> String -> PersistentCommand -> AppContextM (PersistentCommandState, Maybe String)
execute component function
  | component == AppConfigService.cComponent && function == AppConfigService.cInvokeClientCssCompilationName =
    AppConfigService.cInvokeClientCssCompilation
