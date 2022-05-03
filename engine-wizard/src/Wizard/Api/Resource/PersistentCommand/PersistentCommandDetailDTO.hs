module Wizard.Api.Resource.PersistentCommand.PersistentCommandDetailDTO where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Api.Resource.App.AppDTO
import Wizard.Api.Resource.User.UserSuggestionDTO
import Wizard.Model.PersistentCommand.PersistentCommand

data PersistentCommandDetailDTO =
  PersistentCommandDetailDTO
    { _persistentCommandDetailDTOUuid :: U.UUID
    , _persistentCommandDetailDTOState :: PersistentCommandState
    , _persistentCommandDetailDTOComponent :: String
    , _persistentCommandDetailDTOFunction :: String
    , _persistentCommandDetailDTOBody :: String
    , _persistentCommandDetailDTOLastErrorMessage :: Maybe String
    , _persistentCommandDetailDTOAttempts :: Int
    , _persistentCommandDetailDTOMaxAttempts :: Int
    , _persistentCommandDetailDTOApp :: AppDTO
    , _persistentCommandDetailDTOCreatedBy :: Maybe UserSuggestionDTO
    , _persistentCommandDetailDTOCreatedAt :: UTCTime
    , _persistentCommandDetailDTOUpdatedAt :: UTCTime
    }
  deriving (Show, Eq, Generic)
