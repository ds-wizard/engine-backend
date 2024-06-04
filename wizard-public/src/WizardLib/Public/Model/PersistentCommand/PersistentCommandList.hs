module WizardLib.Public.Model.PersistentCommand.PersistentCommandList where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.PersistentCommand.Model.PersistentCommand.PersistentCommand
import WizardLib.Public.Api.Resource.Tenant.TenantSuggestionDTO
import WizardLib.Public.Api.Resource.User.UserSuggestionDTO

data PersistentCommandList = PersistentCommandList
  { uuid :: U.UUID
  , state :: PersistentCommandState
  , component :: String
  , function :: String
  , attempts :: Int
  , maxAttempts :: Int
  , tenant :: TenantSuggestionDTO
  , createdBy :: Maybe UserSuggestionDTO
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
