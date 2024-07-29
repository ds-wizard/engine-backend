module WizardLib.Public.Model.Tenant.Config.TenantConfig where

import GHC.Generics

data TenantConfigDashboardAndLoginScreenAnnouncement = TenantConfigDashboardAndLoginScreenAnnouncement
  { content :: String
  , level :: TenantConfigDashboardAndLoginScreenAnnouncementLevelType
  , loginScreen :: Bool
  , dashboard :: Bool
  }
  deriving (Generic, Eq, Show)

data TenantConfigDashboardAndLoginScreenAnnouncementLevelType
  = InfoAnnouncementLevelType
  | WarningAnnouncementLevelType
  | CriticalAnnouncementLevelType
  deriving (Generic, Eq, Show)
