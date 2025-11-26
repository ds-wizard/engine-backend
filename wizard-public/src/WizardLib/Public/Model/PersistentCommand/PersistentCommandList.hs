module WizardLib.Public.Model.PersistentCommand.PersistentCommandList where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.PersistentCommand.Model.PersistentCommand.PersistentCommand
import WizardLib.Public.Api.Resource.Tenant.TenantSuggestionDTO
import WizardLib.Public.Model.User.UserSuggestion

data PersistentCommandList = PersistentCommandList
  { uuid :: U.UUID
  , state :: PersistentCommandState
  , component :: String
  , function :: String
  , attempts :: Int
  , maxAttempts :: Int
  , tenant :: TenantSuggestionDTO
  , createdBy :: Maybe UserSuggestion
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
