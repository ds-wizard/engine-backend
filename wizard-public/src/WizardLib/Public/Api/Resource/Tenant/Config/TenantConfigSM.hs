module WizardLib.Public.Api.Resource.Tenant.Config.TenantConfigSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import WizardLib.Public.Api.Resource.Tenant.Config.TenantConfigJM ()
import WizardLib.Public.Database.Migration.Development.Tenant.Data.TenantConfigs
import WizardLib.Public.Model.Tenant.Config.TenantConfig

instance ToSchema TenantConfigDashboardAndLoginScreenAnnouncement where
  declareNamedSchema = toSwagger defaultDashboardAndLoginScreenAnnouncement

instance ToSchema TenantConfigDashboardAndLoginScreenAnnouncementLevelType

instance ToSchema TenantConfigLookAndFeel where
  declareNamedSchema = toSwagger defaultLookAndFeel

instance ToSchema TenantConfigLookAndFeelCustomMenuLink where
  declareNamedSchema = toSwagger defaultLookAndFeelCustomLink

instance ToSchema TenantConfigAiAssistant where
  declareNamedSchema = toSwagger defaultAiAssistant
