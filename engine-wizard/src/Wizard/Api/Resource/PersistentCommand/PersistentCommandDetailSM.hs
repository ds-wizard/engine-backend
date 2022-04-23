module Wizard.Api.Resource.PersistentCommand.PersistentCommandDetailSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.App.AppSM ()
import Wizard.Api.Resource.PersistentCommand.PersistentCommandDetailDTO
import Wizard.Api.Resource.PersistentCommand.PersistentCommandDetailJM ()
import Wizard.Api.Resource.PersistentCommand.PersistentCommandSM ()
import Wizard.Api.Resource.User.UserSuggestionSM ()
import Wizard.Database.Migration.Development.App.Data.Apps
import Wizard.Database.Migration.Development.PersistentCommand.Data.PersistentCommands
import Wizard.Database.Migration.Development.User.Data.Users
import qualified Wizard.Service.App.AppMapper as AM
import Wizard.Service.PersistentCommand.PersistentCommandMapper

instance ToSchema PersistentCommandDetailDTO where
  declareNamedSchema = simpleToSchema (toDetailDTO command1 (Just userAlbert) (AM.toDTO defaultApp Nothing Nothing))
