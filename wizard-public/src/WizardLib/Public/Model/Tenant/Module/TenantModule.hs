module WizardLib.Public.Model.Tenant.Module.TenantModule where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data TenantModule = TenantModule
  { tenantUuid :: U.UUID
  , position :: Int
  , moduleKey :: String
  , title :: String
  , description :: String
  , icon :: String
  , url :: String
  , external :: Bool
  , requiredPermission :: Maybe String
  , enabled :: Bool
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Generic, Eq, Show)
