module Registry.Service.PersistentCommand.PersistentCommandMapper where

import Registry.Api.Resource.PersistentCommand.PersistentCommandDTO
import Shared.PersistentCommand.Model.PersistentCommand.PersistentCommand

toDTO :: PersistentCommand String -> PersistentCommandDTO
toDTO command =
  PersistentCommandDTO
    { uuid = command.uuid
    , state = command.state
    , component = command.component
    , function = command.function
    , attempts = command.attempts
    , maxAttempts = command.maxAttempts
    , createdAt = command.createdAt
    , updatedAt = command.updatedAt
    }
