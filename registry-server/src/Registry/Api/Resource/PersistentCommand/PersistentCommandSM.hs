module Registry.Api.Resource.PersistentCommand.PersistentCommandSM where

import Data.Swagger

import Registry.Database.Migration.Development.PersistentCommand.Data.PersistentCommands
import Shared.Common.Util.Swagger
import Shared.PersistentCommand.Api.Resource.PersistentCommand.PersistentCommandSM ()
import Shared.PersistentCommand.Model.PersistentCommand.PersistentCommand

instance ToSchema (PersistentCommand String) where
  declareNamedSchema = toSwagger command1
