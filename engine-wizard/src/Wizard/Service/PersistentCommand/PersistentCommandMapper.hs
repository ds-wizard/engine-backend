module Wizard.Service.PersistentCommand.PersistentCommandMapper where

import Data.Time
import qualified Data.UUID as U

import Wizard.Api.Resource.App.AppDTO
import Wizard.Api.Resource.PersistentCommand.PersistentCommandChangeDTO
import Wizard.Api.Resource.PersistentCommand.PersistentCommandDTO
import Wizard.Api.Resource.PersistentCommand.PersistentCommandDetailDTO
import Wizard.Model.PersistentCommand.PersistentCommand
import Wizard.Model.PersistentCommand.PersistentCommandSimple
import Wizard.Model.User.User
import qualified Wizard.Service.User.UserMapper as U_Mapper

toDTO :: PersistentCommand -> Maybe User -> AppDTO -> PersistentCommandDTO
toDTO command user app =
  PersistentCommandDTO
    { uuid = command.uuid
    , state = command.state
    , component = command.component
    , function = command.function
    , attempts = command.attempts
    , maxAttempts = command.maxAttempts
    , app = app
    , createdBy = fmap (U_Mapper.toSuggestionDTO . U_Mapper.toSuggestion) user
    , createdAt = command.createdAt
    , updatedAt = command.updatedAt
    }

toDetailDTO :: PersistentCommand -> Maybe User -> AppDTO -> PersistentCommandDetailDTO
toDetailDTO command user app =
  PersistentCommandDetailDTO
    { uuid = command.uuid
    , state = command.state
    , component = command.component
    , function = command.function
    , body = command.body
    , lastErrorMessage = command.lastErrorMessage
    , attempts = command.attempts
    , maxAttempts = command.maxAttempts
    , app = app
    , createdBy = fmap (U_Mapper.toSuggestionDTO . U_Mapper.toSuggestion) user
    , createdAt = command.createdAt
    , updatedAt = command.updatedAt
    }

toPersistentCommand
  :: U.UUID -> String -> String -> String -> Int -> Bool -> U.UUID -> Maybe U.UUID -> UTCTime -> PersistentCommand
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

toSimple :: PersistentCommand -> PersistentCommandSimple
toSimple command =
  PersistentCommandSimple
    { uuid = command.uuid
    , appUuid = command.appUuid
    , createdBy = command.createdBy
    }

fromChangeDTO :: PersistentCommand -> PersistentCommandChangeDTO -> UTCTime -> PersistentCommand
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
