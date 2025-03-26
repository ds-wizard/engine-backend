module WizardLib.Public.Model.Tenant.Config.TenantConfig where

import Data.Hashable
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

data TenantConfigOrganization = TenantConfigOrganization
  { name :: String
  , description :: String
  , organizationId :: String
  , affiliations :: [String]
  }
  deriving (Generic, Eq, Show)

instance Hashable TenantConfigOrganization
