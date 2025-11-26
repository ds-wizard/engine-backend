module Wizard.Api.Resource.PersistentCommand.PersistentCommandDetailDTO where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.PersistentCommand.Model.PersistentCommand.PersistentCommand
import Wizard.Api.Resource.Tenant.TenantDTO
import WizardLib.Public.Model.User.UserSuggestion

data PersistentCommandDetailDTO = PersistentCommandDetailDTO
  { uuid :: U.UUID
  , state :: PersistentCommandState
  , component :: String
  , function :: String
  , body :: String
  , lastTraceUuid :: Maybe U.UUID
  , lastErrorMessage :: Maybe String
  , attempts :: Int
  , maxAttempts :: Int
  , tenant :: TenantDTO
  , createdBy :: Maybe UserSuggestion
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
