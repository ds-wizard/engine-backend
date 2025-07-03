module Wizard.Database.Migration.Development.User.Data.NikolaTesla where

import Data.Maybe (fromJust)
import Data.Time

import Shared.Common.Util.Date
import Shared.Common.Util.Uuid
import Wizard.Database.Migration.Development.Tenant.Data.Tenants
import Wizard.Model.Tenant.Tenant
import Wizard.Model.User.User
import Wizard.Service.User.UserMapper
import WizardLib.Public.Api.Resource.User.UserSuggestionDTO
import WizardLib.Public.Database.Migration.Development.User.Data.UserGroups
import WizardLib.Public.Model.User.UserGroup
import WizardLib.Public.Model.User.UserGroupMembership
import WizardLib.Public.Model.User.UserTour

userNikola :: User
userNikola =
  User
    { uuid = u' "30d48cf4-8c8a-496f-bafe-585bd238f798"
    , firstName = "Nikola"
    , lastName = "Tesla"
    , email = "nikola.tesla@example.com"
    , affiliation = Nothing
    , sources = [_USER_SOURCE_INTERNAL]
    , uRole = _USER_ROLE_DATA_STEWARD
    , permissions =
        [ "KM_PERM"
        , "KM_UPGRADE_PERM"
        , "KM_PUBLISH_PERM"
        , "PM_READ_PERM"
        , "PM_WRITE_PERM"
        , "QTN_PERM"
        , "QTN_ACTION_PERM"
        , "QTN_IMPORTER_PERM"
        , "QTN_TML_PERM"
        , "DOC_TML_READ_PERM"
        , "SUBM_PERM"
        , "DOC_TML_WRITE_PERM"
        ]
    , active = True
    , passwordHash = "pbkdf1:sha256|17|awVwfF3h27PrxINtavVgFQ==|iUFbQnZFv+rBXBu1R2OkX+vEjPtohYk5lsyIeOBdEy4="
    , submissionProps = []
    , imageUrl = Nothing
    , locale = Nothing
    , machine = False
    , tenantUuid = defaultTenant.uuid
    , lastVisitedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 26) 0
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 26) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 26) 0
    }

userNikolaBioGroupMembership :: UserGroupMembership
userNikolaBioGroupMembership =
  UserGroupMembership
    { userGroupUuid = bioGroup.uuid
    , userUuid = userNikola.uuid
    , mType = OwnerUserGroupMembershipType
    , tenantUuid = defaultTenant.uuid
    , createdAt = dt' 2018 1 21
    , updatedAt = dt' 2018 1 21
    }

userNikolaTour1 :: UserTour
userNikolaTour1 =
  UserTour
    { userUuid = userNikola.uuid
    , tourId = "TOUR_1"
    , tenantUuid = defaultTenant.uuid
    , createdAt = dt' 2018 1 21
    }

userNikolaSuggestionDto :: UserSuggestionDTO
userNikolaSuggestionDto = toSuggestionDTO . toSuggestion $ userNikola
