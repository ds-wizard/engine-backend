module Wizard.Model.Tenant.PluginSettings.TenantPluginSettings where

import qualified Data.Aeson as A
import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data TenantPluginSettings = TenantPluginSettings
  { tenantUuid :: U.UUID
  , pluginUuid :: U.UUID
  , values :: A.Value
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Generic, Eq, Show)
