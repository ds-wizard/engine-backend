module Wizard.Database.Migration.Development.User.Data.AlbertEinstein where

import Control.Lens ((^.))
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Data.Time

import LensesConfig
import Shared.Util.Uuid
import Wizard.Api.Resource.User.UserPasswordDTO
import Wizard.Api.Resource.User.UserProfileDTO
import Wizard.Api.Resource.User.UserStateDTO
import Wizard.Api.Resource.User.UserSubmissionPropsDTO
import Wizard.Api.Resource.User.UserSuggestionDTO
import Wizard.Database.Migration.Development.Acl.Data.Groups
import Wizard.Database.Migration.Development.App.Data.Apps
import Wizard.Database.Migration.Development.Config.Data.AppConfigs
import Wizard.Model.Common.SensitiveData
import Wizard.Model.User.OnlineUserInfo
import Wizard.Model.User.User
import Wizard.Model.User.UserEM ()
import Wizard.Service.User.UserMapper
import Wizard.Service.User.UserProfileMapper

userAlbert :: User
userAlbert =
  User
    { _userUuid = u' "ec6f8e90-2a91-49ec-aa3f-9eab2267fc66"
    , _userFirstName = "Albert"
    , _userLastName = "Einstein"
    , _userEmail = "albert.einstein@example.com"
    , _userAffiliation = Just "My University"
    , _userSources = [_USER_SOURCE_INTERNAL]
    , _userRole = _USER_ROLE_ADMIN
    , _userPermissions =
        [ "ADMIN_PERM"
        , "UM_PERM"
        , "KM_PERM"
        , "KM_UPGRADE_PERM"
        , "KM_PUBLISH_PERM"
        , "PM_READ_PERM"
        , "PM_WRITE_PERM"
        , "QTN_PERM"
        , "QTN_TML_PERM"
        , "DMP_PERM"
        , "CFG_PERM"
        , "SUBM_PERM"
        , "TML_PERM"
        , "DOC_PERM"
        ]
    , _userActive = True
    , _userPasswordHash = "pbkdf1:sha256|17|awVwfF3h27PrxINtavVgFQ==|iUFbQnZFv+rBXBu1R2OkX+vEjPtohYk5lsyIeOBdEy4="
    , _userSubmissionProps = [userAlbertApiTokenEncrypted]
    , _userImageUrl = Nothing
    , _userGroups = [ownerBioGroup, ownerPlantGroup]
    , _userMachine = False
    , _userAppUuid = defaultApp ^. uuid
    , _userLastVisitedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , _userCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , _userUpdatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    }

userAlbertEdited :: User
userAlbertEdited =
  userAlbert
    { _userFirstName = "EDITED: Isaac"
    , _userLastName = "EDITED: Einstein"
    , _userEmail = "albert.einstein@example-edited.com"
    , _userAffiliation = Just "EDITED: My University"
    , _userSubmissionProps = [userAlbertApiTokenEditedEncrypted]
    }

userAlbertDecrypted :: User
userAlbertDecrypted = process defaultSecret userAlbert

userAlbertProfile :: UserProfileDTO
userAlbertProfile = toUserProfileDTO userAlbert [userAlbertApiTokenDto]

userAlbertProfileEdited :: UserProfileDTO
userAlbertProfileEdited =
  userAlbertProfile
    { _userProfileDTOFirstName = userAlbertEdited ^. firstName
    , _userProfileDTOLastName = userAlbertEdited ^. lastName
    , _userProfileDTOEmail = userAlbertEdited ^. email
    , _userProfileDTOAffiliation = userAlbertEdited ^. affiliation
    , _userProfileDTOSubmissionProps = [userAlbertApiTokenEditedDto]
    }

userPassword :: UserPasswordDTO
userPassword = UserPasswordDTO {_userPasswordDTOPassword = "newPassword"}

userState :: UserStateDTO
userState = UserStateDTO {_userStateDTOActive = True}

userAlbertApiToken :: UserSubmissionProps
userAlbertApiToken =
  UserSubmissionProps
    { _userSubmissionPropsSId = defaultSubmissionService ^. sId
    , _userSubmissionPropsValues = M.fromList [(defaultSubmissionServiceApiTokenProp, "Some Token")]
    }

userAlbertApiTokenEncrypted :: UserSubmissionProps
userAlbertApiTokenEncrypted = process defaultSecret userAlbertApiToken

userAlbertApiTokenDto :: UserSubmissionPropsDTO
userAlbertApiTokenDto =
  UserSubmissionPropsDTO
    { _userSubmissionPropsDTOSId = defaultSubmissionService ^. sId
    , _userSubmissionPropsDTOName = defaultSubmissionService ^. name
    , _userSubmissionPropsDTOValues =
        M.fromList [(defaultSubmissionServiceSecretProp, ""), (defaultSubmissionServiceApiTokenProp, "Some Token")]
    }

userAlbertApiTokenEdited :: UserSubmissionProps
userAlbertApiTokenEdited =
  userAlbertApiToken
    {_userSubmissionPropsValues = M.fromList [(defaultSubmissionServiceApiTokenProp, "EDITED: Some Token")]}

userAlbertApiTokenEditedEncrypted :: UserSubmissionProps
userAlbertApiTokenEditedEncrypted = process defaultSecret userAlbertApiTokenEdited

userAlbertApiTokenEditedDto :: UserSubmissionPropsDTO
userAlbertApiTokenEditedDto =
  UserSubmissionPropsDTO
    { _userSubmissionPropsDTOSId = defaultSubmissionService ^. sId
    , _userSubmissionPropsDTOName = defaultSubmissionService ^. name
    , _userSubmissionPropsDTOValues =
        M.fromList
          [(defaultSubmissionServiceSecretProp, ""), (defaultSubmissionServiceApiTokenProp, "EDITED: Some Token")]
    }

userAlbertOnlineInfo :: OnlineUserInfo
userAlbertOnlineInfo = toLoggedOnlineUserInfo (toDTO userAlbert) 10

userAlbertSuggestion :: UserSuggestionDTO
userAlbertSuggestion = toSuggestionDTO . toSuggestion $ userAlbert
