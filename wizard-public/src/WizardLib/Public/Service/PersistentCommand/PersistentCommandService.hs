module WizardLib.Public.Service.PersistentCommand.PersistentCommandService where

import Control.Monad (void)
import Control.Monad.Reader (ask)
import Data.Foldable (traverse_)
import qualified Data.List as L
import qualified Data.UUID as U
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField

import Shared.Common.Model.Config.ServerConfig
import Shared.Common.Model.Context.AppContext
import Shared.Common.Util.Logger
import Shared.PersistentCommand.Database.DAO.PersistentCommand.PersistentCommandDAO
import Shared.PersistentCommand.Model.PersistentCommand.PersistentCommandSimple
import WizardLib.Public.Database.DAO.PersistentCommand.PersistentCommandDAO

retryPersistentCommandsForLambda :: AppContextC s sc m => m ()
retryPersistentCommandsForLambda = do
  context <- ask
  let components = fmap (\lf -> lf.component) context.serverConfig'.persistentCommand'.lambdaFunctions
  persistentCommands <- findPersistentCommandsForLambdaByStates components
  traverse_ retryPersistentCommandForLambda persistentCommands

retryPersistentCommandForLambda :: (Show identity, FromField identity, ToField identity, AppContextC s sc m) => PersistentCommandSimple identity -> m ()
retryPersistentCommandForLambda command = do
  context <- ask
  case L.find (\lf -> lf.component == command.component) context.serverConfig'.persistentCommand'.lambdaFunctions of
    Just lf -> void $ invokeLambdaFunction command lf
    Nothing -> logWarnI _CMP_DATABASE (f' "No lambda function found for persistent command '%s'" [U.toString command.uuid])
