module Wizard.Api.Resource.PersistentCommand.PersistentCommandDTO where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.PersistentCommand.Model.PersistentCommand.PersistentCommand
import Wizard.Api.Resource.Tenant.TenantDTO
import Wizard.Api.Resource.User.UserSuggestionDTO

data PersistentCommandDTO = PersistentCommandDTO
  { uuid :: U.UUID
  , state :: PersistentCommandState
  , component :: String
  , function :: String
  , attempts :: Int
  , maxAttempts :: Int
  , tenant :: TenantDTO
  , createdBy :: Maybe UserSuggestionDTO
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
