module Shared.PersistentCommand.Service.PersistentCommand.PersistentCommandMapper where

import Data.Time
import qualified Data.UUID as U

import Shared.PersistentCommand.Api.Resource.PersistentCommand.PersistentCommandChangeDTO
import Shared.PersistentCommand.Model.PersistentCommand.PersistentCommand
import Shared.PersistentCommand.Model.PersistentCommand.PersistentCommandSimple

toPersistentCommand
  :: U.UUID -> String -> String -> String -> Int -> Bool -> U.UUID -> Maybe identity -> UTCTime -> PersistentCommand identity
toPersistentCommand uuid component function body maxAttempts internal appUuid mCreatedBy now =
  PersistentCommand
    { uuid = uuid
    , state = NewPersistentCommandState
    , component = component
    , function = function
    , body = body
    , lastErrorMessage = Nothing
    , attempts = 0
    , maxAttempts = maxAttempts
    , internal = internal
    , appUuid = appUuid
    , createdBy = mCreatedBy
    , createdAt = now
    , updatedAt = now
    }

toSimple :: PersistentCommand identity -> PersistentCommandSimple identity
toSimple command =
  PersistentCommandSimple
    { uuid = command.uuid
    , appUuid = command.appUuid
    , createdBy = command.createdBy
    }

fromChangeDTO :: PersistentCommand identity -> PersistentCommandChangeDTO -> UTCTime -> PersistentCommand identity
fromChangeDTO command reqDto now =
  PersistentCommand
    { uuid = command.uuid
    , state = reqDto.state
    , component = command.component
    , function = command.function
    , body = command.body
    , lastErrorMessage = command.lastErrorMessage
    , attempts = command.attempts
    , maxAttempts = command.maxAttempts
    , internal = command.internal
    , appUuid = command.appUuid
    , createdBy = command.createdBy
    , createdAt = command.createdAt
    , updatedAt = now
    }
