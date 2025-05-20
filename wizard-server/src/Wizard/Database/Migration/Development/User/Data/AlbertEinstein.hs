module Wizard.Database.Migration.Development.User.Data.AlbertEinstein where

import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Data.Time

import Shared.Common.Model.Common.SensitiveData
import Shared.Common.Util.Date
import Shared.Common.Util.Uuid
import Shared.Locale.Database.Migration.Development.Locale.Data.Locales
import Shared.Locale.Model.Locale.Locale
import Wizard.Api.Resource.User.UserDTO
import Wizard.Api.Resource.User.UserPasswordDTO
import Wizard.Api.Resource.User.UserProfileChangeDTO
import Wizard.Api.Resource.User.UserStateDTO
import Wizard.Api.Resource.User.UserSubmissionPropsDTO
import Wizard.Database.Migration.Development.Tenant.Data.TenantConfigs
import Wizard.Database.Migration.Development.Tenant.Data.Tenants
import Wizard.Model.Tenant.Config.TenantConfig
import Wizard.Model.Tenant.Tenant
import Wizard.Model.User.OnlineUserInfo
import Wizard.Model.User.User
import Wizard.Model.User.UserEM ()
import Wizard.Model.User.UserProfile
import Wizard.Service.User.UserMapper
import WizardLib.Public.Api.Resource.User.UserSuggestionDTO
import WizardLib.Public.Database.Migration.Development.User.Data.UserGroups
import WizardLib.Public.Model.User.UserGroup
import WizardLib.Public.Model.User.UserGroupMembership
import WizardLib.Public.Model.User.UserTour

userAlbert :: User
userAlbert =
  User
    { uuid = u' "ec6f8e90-2a91-49ec-aa3f-9eab2267fc66"
    , firstName = "Albert"
    , lastName = "Einstein"
    , email = "albert.einstein@example.com"
    , affiliation = Just "My University"
    , sources = [_USER_SOURCE_INTERNAL]
    , uRole = _USER_ROLE_ADMIN
    , permissions =
        [ "TENANT_PERM"
        , "DEV_PERM"
        , "UM_PERM"
        , "KM_PERM"
        , "KM_UPGRADE_PERM"
        , "KM_PUBLISH_PERM"
        , "PM_READ_PERM"
        , "PM_WRITE_PERM"
        , "QTN_PERM"
        , "QTN_FILE_PERM"
        , "QTN_ACTION_PERM"
        , "QTN_IMPORTER_PERM"
        , "QTN_TML_PERM"
        , "DOC_TML_READ_PERM"
        , "CFG_PERM"
        , "SUBM_PERM"
        , "DOC_TML_WRITE_PERM"
        , "DOC_PERM"
        , "LOC_PERM"
        ]
    , active = True
    , passwordHash = "pbkdf1:sha256|17|awVwfF3h27PrxINtavVgFQ==|iUFbQnZFv+rBXBu1R2OkX+vEjPtohYk5lsyIeOBdEy4="
    , submissionProps = [process defaultSecret userAlbertApiToken]
    , imageUrl = Nothing
    , locale = Nothing
    , machine = False
    , tenantUuid = defaultTenant.uuid
    , lastVisitedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    }

userAlbertEdited :: User
userAlbertEdited =
  userAlbert
    { firstName = "EDITED: Isaac"
    , lastName = "EDITED: Einstein"
    , email = "albert.einstein@example-edited.com"
    , affiliation = Just "EDITED: My University"
    }

userAlbertDto :: UserDTO
userAlbertDto = toDTO userAlbert

userAlbertProfile :: UserProfile
userAlbertProfile = toUserProfile (toDTO userAlbert) [bioGroup.uuid]

userAlbertEditedChange :: UserProfileChangeDTO
userAlbertEditedChange =
  UserProfileChangeDTO
    { firstName = userAlbertEdited.firstName
    , lastName = userAlbertEdited.lastName
    , email = userAlbertEdited.email
    , affiliation = userAlbertEdited.affiliation
    }

userAlbertDecrypted :: User
userAlbertDecrypted = process defaultSecret userAlbert

userPassword :: UserPasswordDTO
userPassword = UserPasswordDTO {password = "newPassword"}

userState :: UserStateDTO
userState = UserStateDTO {active = True}

userAlbertOnlineInfo :: OnlineUserInfo
userAlbertOnlineInfo = toLoggedOnlineUserInfo (toDTO userAlbert) 10 [bioGroup.uuid]

userAlbertSuggestion :: UserSuggestionDTO
userAlbertSuggestion = toSuggestionDTO . toSuggestion $ userAlbert

userAlbertBioGroupMembership :: UserGroupMembership
userAlbertBioGroupMembership =
  UserGroupMembership
    { userGroupUuid = bioGroup.uuid
    , userUuid = userAlbert.uuid
    , mType = OwnerUserGroupMembershipType
    , tenantUuid = defaultTenant.uuid
    , createdAt = dt' 2018 1 21
    , updatedAt = dt' 2018 1 21
    }

userAlbertTour1 :: UserTour
userAlbertTour1 =
  UserTour
    { userUuid = userAlbert.uuid
    , tourId = "TOUR_1"
    , tenantUuid = defaultTenant.uuid
    , createdAt = dt' 2018 1 21
    }

userAlbertTour2 :: UserTour
userAlbertTour2 =
  UserTour
    { userUuid = userAlbert.uuid
    , tourId = "TOUR_2"
    , tenantUuid = defaultTenant.uuid
    , createdAt = dt' 2018 1 21
    }

-- --------------------------------------
-- SUBMISSION
-- --------------------------------------
userAlbertEditedSubmission :: User
userAlbertEditedSubmission =
  userAlbert
    { submissionProps = [process defaultSecret userAlbertApiTokenEdited]
    }

userAlbertApiToken :: UserSubmissionProps
userAlbertApiToken =
  UserSubmissionProps
    { sId = defaultSubmissionService.sId
    , values = M.fromList [(defaultSubmissionServiceSecretProp, ""), (defaultSubmissionServiceApiTokenProp, "Some Token")]
    }

userAlbertApiTokenDto :: UserSubmissionPropsDTO
userAlbertApiTokenDto =
  UserSubmissionPropsDTO
    { sId = defaultSubmissionService.sId
    , name = defaultSubmissionService.name
    , values = M.fromList [(defaultSubmissionServiceSecretProp, ""), (defaultSubmissionServiceApiTokenProp, "Some Token")]
    }

userAlbertApiTokenEdited :: UserSubmissionProps
userAlbertApiTokenEdited =
  userAlbertApiToken
    { values = M.fromList [(defaultSubmissionServiceSecretProp, ""), (defaultSubmissionServiceApiTokenProp, "EDITED: Some Token")]
    }

userAlbertApiTokenEditedDto :: UserSubmissionPropsDTO
userAlbertApiTokenEditedDto =
  UserSubmissionPropsDTO
    { sId = defaultSubmissionService.sId
    , name = defaultSubmissionService.name
    , values = M.fromList [(defaultSubmissionServiceSecretProp, ""), (defaultSubmissionServiceApiTokenProp, "EDITED: Some Token")]
    }

userAlbertEditedLocale :: User
userAlbertEditedLocale =
  userAlbert
    { locale = Just localeNl.lId
    }
