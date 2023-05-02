module Registry.Service.PersistentCommand.PersistentCommandMapper where

import Data.Time
import qualified Data.UUID as U

import Registry.Model.PersistentCommand.PersistentCommand

toPersistentCommand
  :: U.UUID -> String -> String -> String -> Int -> Bool -> U.UUID -> String -> UTCTime -> PersistentCommand
toPersistentCommand uuid component function body maxAttempts internal appUuid createdBy now =
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
    , createdBy = createdBy
    , createdAt = now
    , updatedAt = now
    }
