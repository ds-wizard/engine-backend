module Registry.Api.Resource.PersistentCommand.PersistentCommandSM where

import Data.Swagger
import qualified Data.UUID as U

import Registry.Api.Resource.PersistentCommand.PersistentCommandDTO
import Registry.Api.Resource.PersistentCommand.PersistentCommandJM ()
import Registry.Database.Migration.Development.PersistentCommand.Data.PersistentCommands
import Registry.Service.PersistentCommand.PersistentCommandMapper
import Shared.Common.Util.Swagger
import Shared.PersistentCommand.Api.Resource.PersistentCommand.PersistentCommandSM ()
import Shared.PersistentCommand.Model.PersistentCommand.PersistentCommand

instance ToSchema PersistentCommandDTO where
  declareNamedSchema = toSwagger (toDTO command1)

instance ToSchema (PersistentCommand U.UUID) where
  declareNamedSchema = toSwagger command1
