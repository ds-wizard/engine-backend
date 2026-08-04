module Wizard.Database.Migration.Development.User.Data.NicolausCopernicus where

import Data.Maybe (fromJust)
import Data.Time

import Shared.Common.Util.Uuid
import Wizard.Database.Migration.Development.Tenant.Data.Tenants
import Wizard.Database.Migration.Development.User.Data.Roles
import Wizard.Model.Tenant.Tenant
import Wizard.Model.User.User
import WizardLib.Public.Service.User.RoleMapper (toRoleSimple)

userNicolaus :: User
userNicolaus =
  User
    { uuid = u' "d5035272-d0ac-4308-b4cc-3b78f7a61627"
    , firstName = "Nicolaus"
    , lastName = "Copernicus"
    , email = "nicolaus.copernicus@example.com"
    , affiliation = Nothing
    , role = toRoleSimple researcherRole
    , active = True
    , -- cspell:disable
      passwordHash = "pbkdf1:sha256|17|awVwfF3h27PrxINtavVgFQ==|iUFbQnZFv+rBXBu1R2OkX+vEjPtohYk5lsyIeOBdEy4="
    , -- cspell:enable
      imageUrl = Nothing
    , locale = Nothing
    , machine = False
    , lastSeenNewsId = Nothing
    , tenantUuid = defaultTenant.uuid
    , lastVisitedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    , emailVerifiedAt = Just $ UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    , emailPending = Nothing
    }
