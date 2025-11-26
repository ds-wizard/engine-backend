module Wizard.Service.PersistentCommand.PersistentCommandMapper where

import qualified Data.UUID as U

import Shared.PersistentCommand.Model.PersistentCommand.PersistentCommand
import Wizard.Api.Resource.PersistentCommand.PersistentCommandDetailDTO
import Wizard.Api.Resource.Tenant.TenantDTO
import Wizard.Model.User.User
import qualified Wizard.Service.User.UserMapper as U_Mapper

toDetailDTO :: PersistentCommand U.UUID -> Maybe User -> TenantDTO -> PersistentCommandDetailDTO
toDetailDTO command user tenant =
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
    , tenant = tenant
    , createdBy = fmap (U_Mapper.toSuggestion . U_Mapper.toSimple) user
    , createdAt = command.createdAt
    , updatedAt = command.updatedAt
    }
