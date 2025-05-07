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

instance FromJSON TenantConfigOrganization where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantConfigOrganization where
  toJSON = genericToJSON jsonOptions
