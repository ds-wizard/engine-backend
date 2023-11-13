module Wizard.Model.Tenant.Tenant where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data Tenant = Tenant
  { uuid :: U.UUID
  , tenantId :: String
  , name :: String
  , serverDomain :: String
  , serverUrl :: String
  , clientUrl :: String
  , adminServerUrl :: Maybe String
  , adminClientUrl :: Maybe String
  , enabled :: Bool
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
