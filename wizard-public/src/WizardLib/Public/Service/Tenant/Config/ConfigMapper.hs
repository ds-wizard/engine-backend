module WizardLib.Public.Service.Tenant.Config.ConfigMapper where

import Data.Time
import qualified Data.UUID as U

import WizardLib.Public.Api.Resource.Tenant.Config.TenantConfigChangeDTO
import WizardLib.Public.Model.Tenant.Config.TenantConfig

fromDashboardAndLoginScreenAnnouncementChangeDTO :: TenantConfigDashboardAndLoginScreenAnnouncementChangeDTO -> U.UUID -> Int -> UTCTime -> UTCTime -> TenantConfigDashboardAndLoginScreenAnnouncement
fromDashboardAndLoginScreenAnnouncementChangeDTO TenantConfigDashboardAndLoginScreenAnnouncementChangeDTO {..} tenantUuid position createdAt updatedAt = TenantConfigDashboardAndLoginScreenAnnouncement {..}

fromLookAndFeelChangeDTO :: TenantConfigLookAndFeelChangeDTO -> U.UUID -> UTCTime -> UTCTime -> TenantConfigLookAndFeel
fromLookAndFeelChangeDTO a@TenantConfigLookAndFeelChangeDTO {..} tenantUuid createdAt updatedAt =
  let customMenuLinks = zipWith (\i c -> fromLookAndFeelCustomMenuLinkChangeDTO c tenantUuid i createdAt updatedAt) [0 ..] a.customMenuLinks
   in TenantConfigLookAndFeel {..}

fromLookAndFeelCustomMenuLinkChangeDTO :: TenantConfigLookAndFeelCustomMenuLinkChangeDTO -> U.UUID -> Int -> UTCTime -> UTCTime -> TenantConfigLookAndFeelCustomMenuLink
fromLookAndFeelCustomMenuLinkChangeDTO TenantConfigLookAndFeelCustomMenuLinkChangeDTO {..} tenantUuid position createdAt updatedAt = TenantConfigLookAndFeelCustomMenuLink {..}

fromFeaturesChangeFullDTO :: TenantConfigFeaturesChangeFullDTO -> U.UUID -> UTCTime -> UTCTime -> TenantConfigFeatures
fromFeaturesChangeFullDTO TenantConfigFeaturesChangeFullDTO {..} tenantUuid createdAt updatedAt = TenantConfigFeatures {..}
