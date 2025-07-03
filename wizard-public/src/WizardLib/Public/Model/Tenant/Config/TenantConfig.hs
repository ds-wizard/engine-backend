module WizardLib.Public.Model.Tenant.Config.TenantConfig where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.Common.Util.Hashable ()

data TenantConfigDashboardAndLoginScreenAnnouncement = TenantConfigDashboardAndLoginScreenAnnouncement
  { tenantUuid :: U.UUID
  , position :: Int
  , content :: String
  , level :: TenantConfigDashboardAndLoginScreenAnnouncementLevelType
  , loginScreen :: Bool
  , dashboard :: Bool
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Generic, Show)

instance Eq TenantConfigDashboardAndLoginScreenAnnouncement where
  a == b =
    a.tenantUuid == b.tenantUuid
      && a.content == b.content
      && a.level == b.level
      && a.loginScreen == b.loginScreen
      && a.dashboard == b.dashboard

data TenantConfigDashboardAndLoginScreenAnnouncementLevelType
  = InfoAnnouncementLevelType
  | WarningAnnouncementLevelType
  | CriticalAnnouncementLevelType
  deriving (Generic, Eq, Show, Read)

data TenantConfigLookAndFeel = TenantConfigLookAndFeel
  { tenantUuid :: U.UUID
  , appTitle :: Maybe String
  , appTitleShort :: Maybe String
  , customMenuLinks :: [TenantConfigLookAndFeelCustomMenuLink]
  , logoUrl :: Maybe String
  , primaryColor :: Maybe String
  , illustrationsColor :: Maybe String
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Generic, Show)

instance Eq TenantConfigLookAndFeel where
  a == b =
    a.tenantUuid == b.tenantUuid
      && a.appTitle == b.appTitle
      && a.appTitleShort == b.appTitleShort
      && a.customMenuLinks == b.customMenuLinks
      && a.logoUrl == b.logoUrl
      && a.primaryColor == b.primaryColor
      && a.illustrationsColor == b.illustrationsColor

data TenantConfigLookAndFeelCustomMenuLink = TenantConfigLookAndFeelCustomMenuLink
  { tenantUuid :: U.UUID
  , position :: Int
  , icon :: String
  , title :: String
  , url :: String
  , newWindow :: Bool
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Generic)

instance Eq TenantConfigLookAndFeelCustomMenuLink where
  a == b =
    a.tenantUuid == b.tenantUuid
      && a.position == b.position
      && a.icon == b.icon
      && a.title == b.title
      && a.url == b.url
      && a.newWindow == b.newWindow

data TenantConfigAiAssistant = TenantConfigAiAssistant
  { tenantUuid :: U.UUID
  , enabled :: Bool
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

data TenantConfigMail = TenantConfigMail
  { tenantUuid :: U.UUID
  , configUuid :: Maybe U.UUID
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Generic, Show)
