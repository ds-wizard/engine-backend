module WizardLib.Public.Api.Resource.Tenant.Config.TenantConfigChangeSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import WizardLib.Public.Api.Resource.Tenant.Config.TenantConfigChangeDTO
import WizardLib.Public.Api.Resource.Tenant.Config.TenantConfigChangeJM ()
import WizardLib.Public.Api.Resource.Tenant.Config.TenantConfigSM ()
import WizardLib.Public.Database.Migration.Development.Tenant.Data.TenantConfigs

instance ToSchema TenantConfigDashboardAndLoginScreenAnnouncementChangeDTO where
  declareNamedSchema = toSwagger defaultDashboardAndLoginScreenAnnouncementChangeDto

instance ToSchema TenantConfigLookAndFeelChangeDTO where
  declareNamedSchema = toSwagger defaultLookAndFeelChangeDto

instance ToSchema TenantConfigLookAndFeelCustomMenuLinkChangeDTO where
  declareNamedSchema = toSwagger defaultLookAndFeelCustomLinkChangeDto

instance ToSchema TenantConfigAiAssistantChangeDTO where
  declareNamedSchema = toSwagger defaultAiAssistantChangeDto
