module Registry.Api.Resource.PersistentCommand.PersistentCommandSM where

import Data.Swagger
import qualified Data.UUID as U

import Registry.Database.Migration.Development.PersistentCommand.Data.PersistentCommands
import Shared.Common.Util.Swagger
import Shared.PersistentCommand.Api.Resource.PersistentCommand.PersistentCommandSM ()
import Shared.PersistentCommand.Model.PersistentCommand.PersistentCommand

instance ToSchema (PersistentCommand U.UUID) where
  declareNamedSchema = toSwagger command1
