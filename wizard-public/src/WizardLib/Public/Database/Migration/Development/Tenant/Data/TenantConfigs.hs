module WizardLib.Public.Database.Migration.Development.Tenant.Data.TenantConfigs where

import WizardLib.Public.Model.Tenant.Config.TenantConfig

defaultDashboardAndLoginScreenAnnouncement :: TenantConfigDashboardAndLoginScreenAnnouncement
defaultDashboardAndLoginScreenAnnouncement =
  TenantConfigDashboardAndLoginScreenAnnouncement
    { content = "Hello"
    , level = InfoAnnouncementLevelType
    , dashboard = True
    , loginScreen = True
    }

defaultOrganization :: TenantConfigOrganization
defaultOrganization =
  TenantConfigOrganization
    { name = "Organization Amsterdam"
    , description = "Some description of Organization Amsterdam"
    , organizationId = "org.nl.amsterdam"
    , affiliations = []
    }
