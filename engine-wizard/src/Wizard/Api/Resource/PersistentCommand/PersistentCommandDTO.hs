module Wizard.Api.Resource.PersistentCommand.PersistentCommandDTO where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Api.Resource.App.AppDTO
import Wizard.Api.Resource.User.UserSuggestionDTO
import Wizard.Model.PersistentCommand.PersistentCommand

data PersistentCommandDTO =
  PersistentCommandDTO
    { _persistentCommandDTOUuid :: U.UUID
    , _persistentCommandDTOState :: PersistentCommandState
    , _persistentCommandDTOComponent :: String
    , _persistentCommandDTOFunction :: String
    , _persistentCommandDTOAttempts :: Int
    , _persistentCommandDTOMaxAttempts :: Int
    , _persistentCommandDTOApp :: AppDTO
    , _persistentCommandDTOCreatedBy :: Maybe UserSuggestionDTO
    , _persistentCommandDTOCreatedAt :: UTCTime
    , _persistentCommandDTOUpdatedAt :: UTCTime
    }
  deriving (Show, Eq, Generic)
