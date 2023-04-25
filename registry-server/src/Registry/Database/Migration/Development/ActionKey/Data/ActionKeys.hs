module Registry.Database.Migration.Development.ActionKey.Data.ActionKeys where

import Data.Maybe (fromJust)
import Data.Time
import qualified Data.UUID as U

import Registry.Api.Resource.ActionKey.ActionKeyDTO
import Registry.Database.Migration.Development.Organization.Data.Organizations
import Registry.Model.ActionKey.ActionKey
import Registry.Model.Organization.Organization

regActionKey =
  ActionKey
    { uuid = fromJust . U.fromString $ "23f934f2-05b2-45d3-bce9-7675c3f3e5e9"
    , organizationId = orgGlobal.organizationId
    , aType = RegistrationActionKey
    , hash = "1ba90a0f-845e-41c7-9f1c-a55fc5a0554a"
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    }

forgTokActionKey =
  ActionKey
    { uuid = fromJust . U.fromString $ "2728460f-ba9a-4a05-8e47-7faa4dc931bf"
    , organizationId = orgGlobal.organizationId
    , aType = ForgottenTokenActionKey
    , hash = "5b1aff0d-b5e3-436d-b913-6b52d3cbad5f"
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    }

forgTokActionKeyDto =
  ActionKeyDTO {aType = forgTokActionKey.aType, email = orgGlobal.email}
