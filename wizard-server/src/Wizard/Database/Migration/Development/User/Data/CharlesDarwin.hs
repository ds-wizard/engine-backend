module Wizard.Database.Migration.Development.User.Data.CharlesDarwin where

import Data.Maybe (fromJust)
import Data.Time

import Shared.Common.Util.Date
import Shared.Common.Util.Uuid
import Wizard.Database.Migration.Development.Plugin.Data.PluginSettings
import Wizard.Database.Migration.Development.Plugin.Data.Plugins
import Wizard.Database.Migration.Development.Tenant.Data.Tenants
import Wizard.Model.Plugin.Plugin
import Wizard.Model.Tenant.Tenant
import Wizard.Model.User.User
import Wizard.Model.User.UserPluginSettings

userCharles :: User
userCharles =
  User
    { uuid = u' "1693daf3-fc48-4a93-a1fb-385e6c9fe7ac"
    , firstName = "Charles"
    , lastName = "Darwin"
    , email = "charles.darwin@example.com"
    , affiliation = Nothing
    , sources = [_USER_SOURCE_INTERNAL]
    , uRole = _USER_ROLE_RESEARCHER
    , permissions = ["PM_READ_PERM", "PRJ_PERM", "DOC_TML_READ_PERM", "SUBM_PERM"]
    , active = True
    , -- cspell:disable
      passwordHash = "pbkdf1:sha256|17|awVwfF3h27PrxINtavVgFQ==|iUFbQnZFv+rBXBu1R2OkX+vEjPtohYk5lsyIeOBdEy4="
    , -- cspell:enable
      imageUrl = Nothing
    , locale = Nothing
    , machine = False
    , lastSeenNewsId = Nothing
    , tenantUuid = differentTenant.uuid
    , lastVisitedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }

userCharlesPluginSettings :: UserPluginSettings
userCharlesPluginSettings =
  UserPluginSettings
    { userUuid = userCharles.uuid
    , pluginUuid = differentPlugin1.uuid
    , values = plugin1Values2
    , tenantUuid = differentTenant.uuid
    , createdAt = dt' 2018 1 21
    , updatedAt = dt' 2018 1 21
    }
