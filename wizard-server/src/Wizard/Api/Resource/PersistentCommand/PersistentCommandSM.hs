module Wizard.Api.Resource.PersistentCommand.PersistentCommandSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.App.AppSM ()
import Wizard.Api.Resource.PersistentCommand.PersistentCommandDTO
import Wizard.Api.Resource.PersistentCommand.PersistentCommandJM ()
import Wizard.Api.Resource.User.UserSuggestionSM ()
import Wizard.Database.Migration.Development.App.Data.Apps
import Wizard.Database.Migration.Development.PersistentCommand.Data.PersistentCommands
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.PersistentCommand.PersistentCommand
import qualified Wizard.Service.App.AppMapper as AM
import Wizard.Service.PersistentCommand.PersistentCommandMapper

instance ToSchema PersistentCommandState

instance ToSchema PersistentCommandDTO where
  declareNamedSchema = toSwagger (toDTO command1 (Just userAlbert) (AM.toDTO defaultApp Nothing Nothing))

instance ToSchema PersistentCommand where
  declareNamedSchema = toSwagger command1
