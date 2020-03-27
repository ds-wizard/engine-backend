module Wizard.Model.Config.AppConfigEM where

import Shared.Util.Crypto (encryptAES256)
import Wizard.Model.Common.SensitiveData
import Wizard.Model.Config.AppConfig

instance SensitiveData AppConfig where
  process key entity = entity {_appConfigAuth = process key (_appConfigAuth entity)}

instance SensitiveData AppConfigFeatures

instance SensitiveData AppConfigClient

instance SensitiveData AppConfigClientDashboard

instance SensitiveData AppConfigClientCustomMenuLink

instance SensitiveData AppConfigInfo

instance SensitiveData AppConfigAffiliation

instance SensitiveData AppConfigAuth where
  process key entity = entity {_appConfigAuthExternal = process key (_appConfigAuthExternal entity)}

instance SensitiveData AppConfigAuthInternal

instance SensitiveData AppConfigAuthExternal where
  process key entity =
    entity {_appConfigAuthExternalServices = fmap (process key) (_appConfigAuthExternalServices entity)}

instance SensitiveData AppConfigAuthExternalService where
  process key entity =
    entity
      { _appConfigAuthExternalServiceClientId = encryptAES256 key (_appConfigAuthExternalServiceClientId entity)
      , _appConfigAuthExternalServiceClientSecret = encryptAES256 key (_appConfigAuthExternalServiceClientSecret entity)
      }

instance SensitiveData AppConfigAuthExternalServiceParameter

instance SensitiveData AppConfigAuthExternalServiceStyle
