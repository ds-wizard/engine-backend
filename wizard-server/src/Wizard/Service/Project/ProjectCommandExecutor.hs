module Wizard.Service.Project.ProjectCommandExecutor where

import Control.Monad.Except (throwError)
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.UUID as U

import Shared.Common.Model.Error.Error
import Shared.Common.Util.Logger
import Shared.PersistentCommand.Model.PersistentCommand.PersistentCommand
import Wizard.Model.Context.AppContext
import Wizard.Service.Project.ProjectService
import WizardLib.Public.Model.PersistentCommand.Project.CreateProjectCommand

cComponent = "project"

execute :: PersistentCommand U.UUID -> AppContextM (PersistentCommandState, Maybe String)
execute command
  | command.function == cCreateProjectsName = cCreateProjects command
  | otherwise = throwError . GeneralServerError $ "Unknown command function: " <> command.function

cCreateProjectsName = "createProjects"

cCreateProjects :: PersistentCommand U.UUID -> AppContextM (PersistentCommandState, Maybe String)
cCreateProjects persistentCommand = do
  let eCommands = eitherDecode (BSL.pack persistentCommand.body) :: Either String [CreateProjectCommand]
  case eCommands of
    Right commands -> do
      createProjectsFromCommands commands
      return (DonePersistentCommandState, Nothing)
    Left error -> return (ErrorPersistentCommandState, Just $ f' "Problem in deserialization of JSON: %s" [error])
