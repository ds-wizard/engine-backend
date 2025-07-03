module WizardLib.Public.Api.Resource.Tenant.Config.TenantConfigJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import WizardLib.Public.Model.Tenant.Config.TenantConfig

instance FromJSON TenantConfigDashboardAndLoginScreenAnnouncement where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantConfigDashboardAndLoginScreenAnnouncement where
  toJSON = genericToJSON jsonOptions

instance FromJSON TenantConfigDashboardAndLoginScreenAnnouncementLevelType

instance ToJSON TenantConfigDashboardAndLoginScreenAnnouncementLevelType

instance FromJSON TenantConfigLookAndFeel where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantConfigLookAndFeel where
  toJSON = genericToJSON jsonOptions

instance FromJSON TenantConfigLookAndFeelCustomMenuLink where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantConfigLookAndFeelCustomMenuLink where
  toJSON = genericToJSON jsonOptions

instance FromJSON TenantConfigAiAssistant where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantConfigAiAssistant where
  toJSON = genericToJSON jsonOptions
