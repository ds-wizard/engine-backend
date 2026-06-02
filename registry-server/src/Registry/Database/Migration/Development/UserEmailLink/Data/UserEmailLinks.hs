module Registry.Database.Migration.Development.UserEmailLink.Data.UserEmailLinks where

import Data.Maybe (fromJust)
import Data.Time
import qualified Data.UUID as U

import Registry.Model.UserEmailLink.UserEmailLinkType
import RegistryLib.Database.Migration.Development.Organization.Data.Organizations
import RegistryLib.Model.Organization.Organization
import Shared.UserEmailLink.Api.Resource.UserEmailLink.UserEmailLinkDTO
import Shared.UserEmailLink.Model.UserEmailLink.UserEmailLink

registrationUserEmailLink =
  UserEmailLink
    { uuid = fromJust . U.fromString $ "23f934f2-05b2-45d3-bce9-7675c3f3e5e9"
    , identity = orgGlobal.organizationId
    , aType = RegistrationUserEmailLinkType
    , hash = "1ba90a0f-845e-41c7-9f1c-a55fc5a0554a"
    , tenantUuid = U.nil
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    }

forgottenTokenUserEmailLink =
  UserEmailLink
    { uuid = fromJust . U.fromString $ "2728460f-ba9a-4a05-8e47-7faa4dc931bf"
    , identity = orgGlobal.organizationId
    , aType = ForgottenTokenUserEmailLinkType
    , hash = "5b1aff0d-b5e3-436d-b913-6b52d3cbad5f"
    , tenantUuid = U.nil
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    }

forgottenTokenUserEmailLinkDto =
  UserEmailLinkDTO {aType = forgottenTokenUserEmailLink.aType, email = orgGlobal.email}
