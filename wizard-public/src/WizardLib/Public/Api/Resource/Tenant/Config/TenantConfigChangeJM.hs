module WizardLib.Public.Api.Resource.Tenant.Config.TenantConfigChangeJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import WizardLib.Public.Api.Resource.Tenant.Config.TenantConfigChangeDTO
import WizardLib.Public.Api.Resource.Tenant.Config.TenantConfigJM ()

instance FromJSON TenantConfigDashboardAndLoginScreenAnnouncementChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantConfigDashboardAndLoginScreenAnnouncementChangeDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON TenantConfigLookAndFeelChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantConfigLookAndFeelChangeDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON TenantConfigLookAndFeelCustomMenuLinkChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantConfigLookAndFeelCustomMenuLinkChangeDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON TenantConfigAiAssistantChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantConfigAiAssistantChangeDTO where
  toJSON = genericToJSON jsonOptions
