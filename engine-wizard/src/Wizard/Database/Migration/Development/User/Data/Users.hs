module Wizard.Database.Migration.Development.User.Data.Users where

import Control.Lens ((^.))
import Data.Maybe (fromJust)
import Data.Time
import qualified Data.UUID as U

import LensesConfig
import Wizard.Api.Resource.User.UserChangeDTO
import Wizard.Api.Resource.User.UserCreateDTO
import Wizard.Api.Resource.User.UserPasswordDTO
import Wizard.Api.Resource.User.UserProfileChangeDTO
import Wizard.Api.Resource.User.UserStateDTO
import Wizard.Model.User.User

userAlbert :: User
userAlbert =
  User
    { _userUuid = fromJust . U.fromString $ "ec6f8e90-2a91-49ec-aa3f-9eab2267fc66"
    , _userFirstName = "Albert"
    , _userLastName = "Einstein"
    , _userEmail = "albert.einstein@example.com"
    , _userAffiliation = Just "My University"
    , _userSources = [_USER_SOURCE_INTERNAL]
    , _userRole = "ADMIN"
    , _userPermissions =
        [ "UM_PERM"
        , "KM_PERM"
        , "KM_UPGRADE_PERM"
        , "KM_PUBLISH_PERM"
        , "PM_READ_PERM"
        , "PM_WRITE_PERM"
        , "QTN_PERM"
        , "DMP_PERM"
        , "CFG_PERM"
        ]
    , _userActive = True
    , _userPasswordHash = "sha256|17|awVwfF3h27PrxINtavVgFQ==|iUFbQnZFv+rBXBu1R2OkX+vEjPtohYk5lsyIeOBdEy4="
    , _userCreatedAt = Just $ UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , _userUpdatedAt = Just $ UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    }

userNikola :: User
userNikola =
  User
    { _userUuid = fromJust . U.fromString $ "30d48cf4-8c8a-496f-bafe-585bd238f798"
    , _userFirstName = "Nikola"
    , _userLastName = "Tesla"
    , _userEmail = "nikola.tesla@example.com"
    , _userAffiliation = Nothing
    , _userSources = [_USER_SOURCE_INTERNAL]
    , _userRole = "DATASTEWARD"
    , _userPermissions = ["KM_PERM", "KM_UPGRADE_PERM", "KM_PUBLISH_PERM", "PM_READ_PERM", "QTN_PERM", "DMP_PERM"]
    , _userActive = True
    , _userPasswordHash = "sha256|17|awVwfF3h27PrxINtavVgFQ==|iUFbQnZFv+rBXBu1R2OkX+vEjPtohYk5lsyIeOBdEy4="
    , _userCreatedAt = Just $ UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , _userUpdatedAt = Just $ UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    }

userIsaac :: User
userIsaac =
  User
    { _userUuid = fromJust . U.fromString $ "e1c58e52-0824-4526-8ebe-ec38eec67030"
    , _userFirstName = "Isaac"
    , _userLastName = "Newton"
    , _userEmail = "isaac.newton@example.com"
    , _userAffiliation = Nothing
    , _userSources = [_USER_SOURCE_INTERNAL]
    , _userRole = "RESEARCHER"
    , _userPermissions = ["PM_READ_PERM", "QTN_PERM", "DMP_PERM"]
    , _userActive = True
    , _userPasswordHash = "sha256|17|awVwfF3h27PrxINtavVgFQ==|iUFbQnZFv+rBXBu1R2OkX+vEjPtohYk5lsyIeOBdEy4="
    , _userCreatedAt = Just $ UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , _userUpdatedAt = Just $ UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    }

userJohnCreate :: UserCreateDTO
userJohnCreate =
  UserCreateDTO
    { _userCreateDTOFirstName = "John"
    , _userCreateDTOLastName = "Doe"
    , _userCreateDTOEmail = "john.doe@example.com"
    , _userCreateDTOAffiliation = Just "My University"
    , _userCreateDTORole = Just "ADMIN"
    , _userCreateDTOPassword = "password"
    }

userIsaacChange :: UserChangeDTO
userIsaacChange =
  UserChangeDTO
    { _userChangeDTOUuid = userAlbert ^. uuid
    , _userChangeDTOFirstName = "EDITED: Isaac"
    , _userChangeDTOLastName = "EDITED: Newton"
    , _userChangeDTOEmail = "albert.einstein@example.com"
    , _userChangeDTOAffiliation = Just "EDITED: My University"
    , _userChangeDTORole = "ADMIN"
    , _userChangeDTOActive = True
    }

userIsaacProfileChange :: UserProfileChangeDTO
userIsaacProfileChange =
  UserProfileChangeDTO
    { _userProfileChangeDTOFirstName = "EDITED: Isaac"
    , _userProfileChangeDTOLastName = "EDITED: Newton"
    , _userProfileChangeDTOEmail = "isaac.newton@example-edited.com"
    , _userProfileChangeDTOAffiliation = Just "EDITED: My University"
    }

userPassword :: UserPasswordDTO
userPassword = UserPasswordDTO {_userPasswordDTOPassword = "newPassword"}

userState :: UserStateDTO
userState = UserStateDTO {_userStateDTOActive = True}
