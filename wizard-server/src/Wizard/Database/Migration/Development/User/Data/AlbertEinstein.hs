module Wizard.Database.Migration.Development.User.Data.AlbertEinstein where

import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Data.Time

import Shared.Common.Model.Common.SensitiveData
import Shared.Common.Util.Uuid
import Wizard.Api.Resource.User.UserPasswordDTO
import Wizard.Api.Resource.User.UserProfileChangeDTO
import Wizard.Api.Resource.User.UserStateDTO
import Wizard.Api.Resource.User.UserSubmissionPropsDTO
import Wizard.Api.Resource.User.UserSuggestionDTO
import Wizard.Database.Migration.Development.Acl.Data.Groups
import Wizard.Database.Migration.Development.App.Data.Apps
import Wizard.Database.Migration.Development.Config.Data.AppConfigs
import Wizard.Model.App.App
import Wizard.Model.Config.AppConfig
import Wizard.Model.User.OnlineUserInfo
import Wizard.Model.User.User
import Wizard.Model.User.UserEM ()
import Wizard.Service.User.UserMapper

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
        [ "APP_PERM"
        , "DEV_PERM"
        , "UM_PERM"
        , "KM_PERM"
        , "KM_UPGRADE_PERM"
        , "KM_PUBLISH_PERM"
        , "PM_READ_PERM"
        , "PM_WRITE_PERM"
        , "QTN_PERM"
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
    , groups = [ownerBioGroup, ownerPlantGroup]
    , machine = False
    , appUuid = defaultApp.uuid
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
userAlbertOnlineInfo = toLoggedOnlineUserInfo (toDTO userAlbert) 10

userAlbertSuggestion :: UserSuggestionDTO
userAlbertSuggestion = toSuggestionDTO . toSuggestion $ userAlbert

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
