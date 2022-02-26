module Wizard.Service.PersistentCommand.PersistentCommandMapper where

import Data.Time
import qualified Data.UUID as U

import Wizard.Model.PersistentCommand.PersistentCommand

toPersistentCommand ::
     U.UUID -> String -> String -> String -> Int -> Bool -> U.UUID -> U.UUID -> UTCTime -> PersistentCommand
toPersistentCommand uuid component function body maxAttempts internal appUuid createdBy now =
  PersistentCommand
    { _persistentCommandUuid = uuid
    , _persistentCommandState = NewPersistentCommandState
    , _persistentCommandComponent = component
    , _persistentCommandFunction = function
    , _persistentCommandBody = body
    , _persistentCommandLastErrorMessage = Nothing
    , _persistentCommandAttempts = 0
    , _persistentCommandMaxAttempts = maxAttempts
    , _persistentCommandInternal = internal
    , _persistentCommandAppUuid = appUuid
    , _persistentCommandCreatedBy = createdBy
    , _persistentCommandCreatedAt = now
    , _persistentCommandUpdatedAt = now
    }
