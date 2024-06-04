module Wizard.Api.Resource.PersistentCommand.PersistentCommandSM where

import Data.Swagger
import qualified Data.UUID as U

import Shared.Common.Util.Swagger
import Shared.PersistentCommand.Api.Resource.PersistentCommand.PersistentCommandSM ()
import Shared.PersistentCommand.Model.PersistentCommand.PersistentCommand
import Wizard.Database.Migration.Development.PersistentCommand.Data.PersistentCommands

instance ToSchema (PersistentCommand U.UUID) where
  declareNamedSchema = toSwagger command1
