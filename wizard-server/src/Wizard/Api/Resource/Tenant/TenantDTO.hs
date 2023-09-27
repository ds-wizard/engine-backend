module Wizard.Api.Resource.Tenant.TenantDTO where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data TenantDTO = TenantDTO
  { uuid :: U.UUID
  , tenantId :: String
  , name :: String
  , serverDomain :: String
  , serverUrl :: String
  , clientUrl :: String
  , enabled :: Bool
  , logoUrl :: Maybe String
  , primaryColor :: Maybe String
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
