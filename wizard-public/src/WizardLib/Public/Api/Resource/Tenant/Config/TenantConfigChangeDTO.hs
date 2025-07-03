module WizardLib.Public.Api.Resource.Tenant.Config.TenantConfigChangeDTO where

import GHC.Generics

import WizardLib.Public.Model.Tenant.Config.TenantConfig

data TenantConfigDashboardAndLoginScreenAnnouncementChangeDTO = TenantConfigDashboardAndLoginScreenAnnouncementChangeDTO
  { content :: String
  , level :: TenantConfigDashboardAndLoginScreenAnnouncementLevelType
  , loginScreen :: Bool
  , dashboard :: Bool
  }
  deriving (Generic, Eq, Show)

data TenantConfigLookAndFeelChangeDTO = TenantConfigLookAndFeelChangeDTO
  { appTitle :: Maybe String
  , appTitleShort :: Maybe String
  , customMenuLinks :: [TenantConfigLookAndFeelCustomMenuLinkChangeDTO]
  , logoUrl :: Maybe String
  , primaryColor :: Maybe String
  , illustrationsColor :: Maybe String
  }
  deriving (Generic, Eq, Show)

data TenantConfigLookAndFeelCustomMenuLinkChangeDTO = TenantConfigLookAndFeelCustomMenuLinkChangeDTO
  { icon :: String
  , title :: String
  , url :: String
  , newWindow :: Bool
  }
  deriving (Show, Eq, Generic)

data TenantConfigAiAssistantChangeDTO = TenantConfigAiAssistantChangeDTO
  { enabled :: Bool
  }
  deriving (Show, Eq, Generic)
