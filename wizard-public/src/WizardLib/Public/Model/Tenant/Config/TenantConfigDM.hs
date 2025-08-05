module WizardLib.Public.Model.Tenant.Config.TenantConfigDM where

import qualified Data.UUID as U

import Shared.Common.Util.Date
import WizardLib.Public.Model.Tenant.Config.TenantConfig

defaultLookAndFeel :: TenantConfigLookAndFeel
defaultLookAndFeel =
  TenantConfigLookAndFeel
    { tenantUuid = U.nil
    , appTitle = Nothing
    , appTitleShort = Nothing
    , customMenuLinks = []
    , logoUrl = Nothing
    , primaryColor = Nothing
    , illustrationsColor = Nothing
    , createdAt = dt' 2018 1 20
    , updatedAt = dt' 2018 1 20
    }

defaultFeatures :: TenantConfigFeatures
defaultFeatures =
  TenantConfigFeatures
    { tenantUuid = U.nil
    , aiAssistantEnabled = True
    , toursEnabled = True
    , createdAt = dt' 2018 1 20
    , updatedAt = dt' 2018 1 20
    }

defaultMail :: TenantConfigMail
defaultMail =
  TenantConfigMail
    { tenantUuid = U.nil
    , configUuid = Nothing
    , createdAt = dt' 2018 1 20
    , updatedAt = dt' 2018 1 20
    }
