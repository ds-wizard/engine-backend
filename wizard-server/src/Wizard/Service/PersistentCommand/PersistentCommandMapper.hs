module Wizard.Service.PersistentCommand.PersistentCommandMapper where

import qualified Data.UUID as U

import Shared.PersistentCommand.Model.PersistentCommand.PersistentCommand
import Wizard.Api.Resource.App.AppDTO
import Wizard.Api.Resource.PersistentCommand.PersistentCommandDTO
import Wizard.Api.Resource.PersistentCommand.PersistentCommandDetailDTO
import Wizard.Model.User.User
import qualified Wizard.Service.User.UserMapper as U_Mapper

toDTO :: PersistentCommand U.UUID -> Maybe User -> AppDTO -> PersistentCommandDTO
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

toDetailDTO :: PersistentCommand U.UUID -> Maybe User -> AppDTO -> PersistentCommandDetailDTO
toDetailDTO command user app =
  PersistentCommandDetailDTO
    { uuid = command.uuid
    , state = command.state
    , component = command.component
    , function = command.function
    , body = command.body
    , lastTraceUuid = command.lastTraceUuid
    , lastErrorMessage = command.lastErrorMessage
    , attempts = command.attempts
    , maxAttempts = command.maxAttempts
    , app = app
    , createdBy = fmap (U_Mapper.toSuggestionDTO . U_Mapper.toSuggestion) user
    , createdAt = command.createdAt
    , updatedAt = command.updatedAt
    }
