module Wizard.Database.Migration.Development.ActionKey.Data.ActionKeys where

import Data.Maybe (fromJust)
import Data.Time

import Shared.ActionKey.Api.Resource.ActionKey.ActionKeyDTO
import Shared.ActionKey.Model.ActionKey.ActionKey
import Shared.Common.Util.Uuid
import Wizard.Database.Migration.Development.Tenant.Data.Tenants
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.ActionKey.ActionKeyType
import Wizard.Model.Tenant.Tenant
import Wizard.Model.User.User

registrationActionKey =
  ActionKey
    { uuid = u' "23f934f2-05b2-45d3-bce9-7675c3f3e5e9"
    , identity = userAlbert.uuid
    , aType = RegistrationActionKey
    , hash = "1ba90a0f-845e-41c7-9f1c-a55fc5a0554a"
    , tenantUuid = defaultTenant.uuid
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    }

forgottenPasswordActionKey =
  ActionKey
    { uuid = u' "23f934f2-05b2-45d3-bce9-7675c3f3e5e9"
    , identity = userAlbert.uuid
    , aType = ForgottenPasswordActionKey
    , hash = "1ba90a0f-845e-41c7-9f1c-a55fc5a0554a"
    , tenantUuid = defaultTenant.uuid
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    }

forgottenPasswordActionKeyDto =
  ActionKeyDTO {aType = forgottenPasswordActionKey.aType, email = userAlbert.email}

differentActionKey =
  ActionKey
    { uuid = u' "61feb6c8-3be6-4095-b2e8-7e63dcfd1f31"
    , identity = userCharles.uuid
    , aType = RegistrationActionKey
    , hash = "b2da34b1-35b2-408b-8127-b0ab3b8b04d9"
    , tenantUuid = differentTenant.uuid
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    }
