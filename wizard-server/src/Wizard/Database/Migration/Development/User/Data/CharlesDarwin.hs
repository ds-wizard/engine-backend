module Wizard.Database.Migration.Development.User.Data.CharlesDarwin where

import Data.Maybe (fromJust)
import Data.Time

import Shared.Common.Util.Date
import Shared.Common.Util.Uuid
import Wizard.Database.Migration.Development.Plugin.Data.PluginSettings
import Wizard.Database.Migration.Development.Plugin.Data.Plugins
import Wizard.Database.Migration.Development.Tenant.Data.Tenants
import Wizard.Database.Migration.Development.User.Data.Roles
import Wizard.Model.Plugin.Plugin
import Wizard.Model.Tenant.Tenant
import Wizard.Model.User.User
import Wizard.Model.User.UserPluginSettings
import WizardLib.Public.Model.User.Role
import WizardLib.Public.Service.User.RoleMapper (toRoleSimple)

userCharles :: User
userCharles =
  User
    { uuid = u' "1693daf3-fc48-4a93-a1fb-385e6c9fe7ac"
    , firstName = "Charles"
    , lastName = "Darwin"
    , email = "charles.darwin@example.com"
    , affiliation = Nothing
    , role = (toRoleSimple researcherRole) {uuid = differentResearcherRole.uuid}
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
    , emailVerifiedAt = Just $ UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    , emailPending = Nothing
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
