module Shared.PersistentCommand.Api.Resource.PersistentCommand.PersistentCommandChangeSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Shared.PersistentCommand.Api.Resource.PersistentCommand.PersistentCommandChangeDTO
import Shared.PersistentCommand.Api.Resource.PersistentCommand.PersistentCommandChangeJM ()
import Shared.PersistentCommand.Api.Resource.PersistentCommand.PersistentCommandSM ()
import Shared.PersistentCommand.Model.PersistentCommand.PersistentCommand

instance ToSchema PersistentCommandChangeDTO where
  declareNamedSchema = toSwagger (PersistentCommandChangeDTO {state = IgnorePersistentCommandState})
