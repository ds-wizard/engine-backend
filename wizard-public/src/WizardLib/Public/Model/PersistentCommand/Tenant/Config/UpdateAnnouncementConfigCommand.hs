module WizardLib.Public.Model.PersistentCommand.Tenant.Config.UpdateAnnouncementConfigCommand where

import Data.Aeson
import GHC.Generics

import Shared.Common.Util.Aeson
import WizardLib.Public.Api.Resource.Tenant.Config.TenantConfigJM ()
import WizardLib.Public.Model.Tenant.Config.TenantConfig

data UpdateAnnouncementConfigCommand = UpdateAnnouncementConfigCommand
  { announcements :: [TenantConfigDashboardAndLoginScreenAnnouncement]
  }
  deriving (Show, Eq, Generic)

instance FromJSON UpdateAnnouncementConfigCommand where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON UpdateAnnouncementConfigCommand where
  toJSON = genericToJSON jsonOptions
