module Wizard.Service.PersistentCommand.PersistentCommandMapper where

import Control.Lens ((^.))
import Data.Time
import qualified Data.UUID as U

import LensesConfig
import Wizard.Api.Resource.App.AppDTO
import Wizard.Api.Resource.PersistentCommand.PersistentCommandDTO
import Wizard.Api.Resource.PersistentCommand.PersistentCommandDetailDTO
import Wizard.Model.PersistentCommand.PersistentCommand
import Wizard.Model.PersistentCommand.PersistentCommandSimple
import Wizard.Model.User.User
import qualified Wizard.Service.User.UserMapper as U_Mapper

toDTO :: PersistentCommand -> Maybe User -> AppDTO -> PersistentCommandDTO
toDTO command user app =
  PersistentCommandDTO
    { _persistentCommandDTOUuid = command ^. uuid
    , _persistentCommandDTOState = command ^. state
    , _persistentCommandDTOComponent = command ^. component
    , _persistentCommandDTOFunction = command ^. function
    , _persistentCommandDTOAttempts = command ^. attempts
    , _persistentCommandDTOMaxAttempts = command ^. maxAttempts
    , _persistentCommandDTOApp = app
    , _persistentCommandDTOCreatedBy = fmap (U_Mapper.toSuggestionDTO . U_Mapper.toSuggestion) user
    , _persistentCommandDTOCreatedAt = command ^. createdAt
    , _persistentCommandDTOUpdatedAt = command ^. updatedAt
    }

toDetailDTO :: PersistentCommand -> Maybe User -> AppDTO -> PersistentCommandDetailDTO
toDetailDTO command user app =
  PersistentCommandDetailDTO
    { _persistentCommandDetailDTOUuid = command ^. uuid
    , _persistentCommandDetailDTOState = command ^. state
    , _persistentCommandDetailDTOComponent = command ^. component
    , _persistentCommandDetailDTOFunction = command ^. function
    , _persistentCommandDetailDTOBody = command ^. body
    , _persistentCommandDetailDTOLastErrorMessage = command ^. lastErrorMessage
    , _persistentCommandDetailDTOAttempts = command ^. attempts
    , _persistentCommandDetailDTOMaxAttempts = command ^. maxAttempts
    , _persistentCommandDetailDTOApp = app
    , _persistentCommandDetailDTOCreatedBy = fmap (U_Mapper.toSuggestionDTO . U_Mapper.toSuggestion) user
    , _persistentCommandDetailDTOCreatedAt = command ^. createdAt
    , _persistentCommandDetailDTOUpdatedAt = command ^. updatedAt
    }

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

toSimple :: PersistentCommand -> PersistentCommandSimple
toSimple command =
  PersistentCommandSimple
    { _persistentCommandSimpleUuid = command ^. uuid
    , _persistentCommandSimpleAppUuid = command ^. appUuid
    , _persistentCommandSimpleCreatedBy = command ^. createdBy
    }
